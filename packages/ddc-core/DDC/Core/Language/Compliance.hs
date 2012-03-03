
module DDC.Core.Language.Compliance
        ( complies
        , Complies(..))
where
import DDC.Core.Language.Feature
import DDC.Core.Language.Profile
import DDC.Core.Exp
import DDC.Type.Env             (Env)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set

complies 
        :: forall n c. (Ord n, Complies c)
        => Profile n -> c n -> Maybe (Error n)
complies = result `seq` undefined


class Complies (c :: * -> *) where
 -- Check compliance of a well typed term with a language profile.
 -- If it is not well typed then this can return a bad result.
 compliesX
        :: forall n. Ord n 
        => Profile n 
        -> Env n                -- Kind environment.
        -> Env n                -- Type environment.
        -> c n 
        -> CheckM n
                (Set n, Set n)  -- Used type and value names.


instance Complies (Exp a) where
 compliesX profile kenv tenv xx
  = let has f   = f $ profileFeatures profile
    in case xx of
        -- variables ----------------------------
        XVar _ (UName n _)
         ->     return (Set.empty, Set.singleton n)

        XVar _ u@(UPrim n _)
         |  Env.member u (profilePrimTypes profile)
         -> return (Set.empty, Set.empty)

         |  otherwise            
         -> throw $ ErrorUndefinedPrim n

        -- constructors -------------------------
        XCon{}
         | has featuresDataCtors -> return (Set.empty, Set.empty)
         | otherwise             -> throw $ ErrorUnsupported DataCtors

        -- spec binders -------------------------
        -- TODO: check for shadowing
        -- TODO: check for deburuijn binders
        XLAM _ b x
         -> do  (tUsed, vUsed) <- compliesX profile (Env.extend b kenv) tenv x

                tUsed'
                 <- case b of
                     BName n _
--                    | not $ Set.member n tUsed -> throw  $ ErrorUnusedBind n
                      | otherwise                -> return $ Set.delete n tUsed
                     _                           -> return tUsed

                return (tUsed', vUsed)

        -- value and witness abstraction --------
        -- TODO: check for shadowing
        -- TODO: check for nested functions
        -- TODO: check for debruijn binders
        XLam _ b x
         -> do  (tUsed, vUsed) <- compliesX profile kenv (Env.extend b tenv) x

                vUsed'
                 <- case b of
                     BName n _
                      | not $ Set.member n vUsed 
                      , not $ has featuresUnusedBindings 
                      -> throw  $ ErrorUnusedBind n

                      | otherwise       -> return $ Set.delete n vUsed
                     _                  -> return vUsed

                return (tUsed, vUsed')
       
        -- application --------------------------
        -- TODO: check for general application
        -- TODO: check for partial application
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

                -- Check for unsupported lazy bindings.
                (case mode of
                  LetStrict     -> return ()
                  LetLazy _     
                   | has featuresLazyBindings 
                   -> throw $ ErrorUnsupported LazyBindings
                   | otherwise  -> return ())

                -- Check that the bound variable is used.
                vUsed2'
                 <- case b1 of
                     BName n _ 
                      | not $ Set.member n vUsed2 
                      , not $ has featuresUnusedBindings
                      -> throw $ ErrorUnusedBind n

                      | otherwise       -> return $ Set.delete n vUsed2
                     _                  -> return vUsed2

                return  ( Set.union tUsed1 tUsed2
                        , Set.union vUsed1 vUsed2')


        _ -> error "compliesX: not finished"


-- Error ----------------------------------------------------------------------
data Error n
        = ErrorUnsupported      Feature
        | ErrorUndefinedPrim    n 
        | ErrorShadow           n
        | ErrorUnusedBind       n
        | ErrorUnusedMatch      n
        | ErrorUnusedImport     n
        deriving (Eq, Show)


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
