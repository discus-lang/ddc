
module DDC.Core.Language.Compliance
        ( complies
        , Complies      (..)
        , Error         (..))
where
import DDC.Core.Language.Feature
import DDC.Core.Language.Profile
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.Monad
import DDC.Type.Env             (Env)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set

complies 
        :: forall n c. (Ord n, Show n, Complies c)
        => Profile n -> c n -> Maybe (Error n)
complies profile thing
 = let  merr    = result $ compliesX 
                        profile 
                        (profilePrimKinds profile)
                        (profilePrimTypes profile)
                        thing
   in   case merr of
         Left err -> Just err
         Right _  -> Nothing


class Complies (c :: * -> *) where
 -- Check compliance of a well typed term with a language profile.
 -- If it is not well typed then this can return a bad result.
 compliesX
        :: forall n. (Ord n, Show n)
        => Profile n 
        -> Env n                -- Kind environment.
        -> Env n                -- Type environment.
        -> c n 
        -> CheckM n
                (Set n, Set n)  -- Used type and value names.


instance Show a => Complies (Module a) where
 compliesX profile kenv tenv mm
  = compliesX profile kenv tenv (moduleBody mm)


instance Show a => Complies (Exp a) where
 compliesX profile kenv tenv xx
  = let has f   = f $ profileFeatures profile
        ok      = return (Set.empty, Set.empty)
    in case xx of
        -- variables ----------------------------
        XVar _ (UName n _)
         ->     return (Set.empty, Set.singleton n)

        XVar _ u@(UPrim n _)
         |  Env.member u (profilePrimTypes profile)
         -> return (Set.empty, Set.empty)

         |  otherwise            
         -> throw $ ErrorUndefinedPrim n

        XVar _ _
         -> ok

        -- constructors -------------------------
        XCon _ UPrim{}           -> ok
        XCon _ _                
         | has featuresDataCtors -> ok
         | otherwise             -> throw $ ErrorUnsupported DataCtors

        -- spec binders -------------------------
        -- TODO: check for deburuijn binders
        XLAM _ b x
         -> do  (tUsed, vUsed)  <- compliesX profile (Env.extend b kenv) tenv x
                tUsed'          <- checkBind profile kenv b tUsed
                return (tUsed', vUsed)

        -- value and witness abstraction --------
        -- TODO: check for nested functions
        -- TODO: check for debruijn binders
        XLam _ b x
         -> do  (tUsed, vUsed)  <- compliesX profile kenv (Env.extend b tenv) x
                vUsed'          <- checkBind profile tenv b vUsed
                return (tUsed, vUsed')
       
        -- application --------------------------
        -- TODO: check for general application
        -- TODO: check for partial application
        XApp _ x1 XType{}
         ->     compliesX profile kenv tenv x1

        XApp _ x1 XWitness{}
         ->     compliesX profile kenv tenv x1

        XApp _ x1 x2
         -> do  (tUsed1, vUsed1) <- compliesX profile kenv tenv x1
                (tUsed2, vUsed2) <- compliesX profile kenv tenv x2
                return  ( Set.union tUsed1 tUsed2
                        , Set.union vUsed1 vUsed2)

        -- let ----------------------------------
        -- TODO: check for debruijn binders.
        XLet _ (LLet mode b1 x1) x2
         -> do  let tenv'        = Env.extend b1 tenv
                (tUsed1, vUsed1) <- compliesX profile kenv tenv  x1
                (tUsed2, vUsed2) <- compliesX profile kenv tenv' x2
                vUsed2'          <- checkBind profile tenv b1 vUsed2

                -- Check for unsupported lazy bindings.
                (case mode of
                  LetStrict     -> return ()
                  LetLazy _     
                   | has featuresLazyBindings -> return ()
                   | otherwise          
                   -> throw $ ErrorUnsupported LazyBindings)

                return  ( Set.union tUsed1 tUsed2
                        , Set.union vUsed1 vUsed2')

        XLet _ (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                let tenv'       = Env.extends bs tenv

                (tUseds1, vUseds1) <- liftM unzip
                                   $  mapM (compliesX profile kenv tenv') xs

                (tUsed2,  vUsed2)  <- compliesX profile kenv tenv' x2
                let tUseds      = Set.unions (tUsed2 : tUseds1)
                let vUseds      = Set.unions (vUsed2 : vUseds1)

                vUseds'            <- checkBinds profile tenv bs vUseds
                return (tUseds, vUseds')


        -- TODO: check for unused binders
        XLet _ LLetRegion{} x2
         -> do  (tUsed2, vUsed2) <- compliesX profile kenv tenv x2
                return (tUsed2, vUsed2)

        XLet _ LWithRegion{} x2
         -> do  (tUsed2, vUsed2) <- compliesX profile kenv tenv x2
                return (tUsed2, vUsed2)

        -- case ---------------------------------
        XCase _ x1 alts
         -> do  (tUsed1,  vUsed1)  <- compliesX profile kenv tenv x1
                (tUseds2, vUseds2) <- liftM unzip 
                                   $  mapM (compliesX profile kenv tenv) alts

                return  ( Set.unions $ tUsed1 : tUseds2
                        , Set.unions $ vUsed1 : vUseds2)


        -- cast ---------------------------------
        XCast _ _ x     -> compliesX profile kenv tenv x

        -- type and witness ---------------------
        XType t         -> throw $ ErrorNakedType    t
        XWitness w      -> throw $ ErrorNakedWitness w


instance Show a => Complies (Alt a) where
 compliesX profile kenv tenv aa
  = case aa of
        AAlt PDefault x
         -> do  (tUsed1, vUsed1)  <- compliesX profile kenv tenv x
                return  (tUsed1, vUsed1)

        AAlt (PData _ bs) x
         -> do  (tUsed1, vUsed1) <- compliesX profile kenv (Env.extends bs tenv) x
                vUsed1'          <- checkBinds profile tenv bs vUsed1 
                return (tUsed1, vUsed1')


-- | Check for compliance violations at a binding site.
checkBind 
        :: Ord n 
        => Profile n            -- ^ The current language profile.
        -> Env n                -- ^ The current environment
        -> Bind n               -- ^ The binder at this site.
        -> Set n                -- ^ Names used under the binder.
        -> CheckM n (Set n)     -- ^ Names used above the binder.

checkBind profile env bb used
 = let has f   = f $ profileFeatures profile
   in case bb of
        BName n _
         | not $ Set.member n used
         , not $ has featuresUnusedBindings 
         -> throw $ ErrorUnusedBind n

         | Env.memberBind bb env
         , not $ has featuresNameShadowing 
         -> throw $ ErrorShadowedBind n

         | otherwise
         -> return $ Set.delete n used

        _ -> return used


-- | Check for compliance violations at a binding site.
--   The binders must all be at the same level.
checkBinds 
        :: Ord n  
        => Profile n 
        -> Env n  -> [Bind n] -> Set n 
        -> CheckM n (Set n)

checkBinds profile env bs used
 = case bs of
        []              -> return used
        (b : bs')        
         -> do  used'   <- checkBinds profile env bs' used
                checkBind profile env b used'


-- Error ----------------------------------------------------------------------
data Error n
        = ErrorUnsupported      Feature
        | ErrorUndefinedPrim    n 
        | ErrorShadowedBind     n
        | ErrorUnusedBind       n
        | ErrorNakedType        (Type    n)
        | ErrorNakedWitness     (Witness n)
        deriving (Eq, Show)

instance Show n => Pretty (Error n) where
 ppr err        = text (show err)


-- Monad ----------------------------------------------------------------------
-- | Compliance checking monad.
data CheckM n x
        = CheckM (Either (Error n) x)

instance Monad (CheckM n) where
 return x   = CheckM (Right x)
 (>>=) m f  
  = case m of
          CheckM (Left err)     -> CheckM (Left err)
          CheckM (Right x)      -> f x


-- | Throw an error in the monad.
throw :: Error n -> CheckM n x
throw e       = CheckM $ Left e


-- | Take the result from a check monad.
result :: CheckM n x -> Either (Error n) x
result (CheckM r)       = r
