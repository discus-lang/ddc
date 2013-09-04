
module DDC.Core.Fragment.Compliance
        ( complies
	, compliesWithEnvs
        , Complies)
where
import DDC.Core.Fragment.Feature
import DDC.Core.Fragment.Profile
import DDC.Core.Fragment.Error
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Module
import DDC.Core.Exp
import Control.Monad
import Data.Maybe
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map


-- | Check whether a core thing complies with a language fragment profile.
complies 
        :: (Ord n, Show n, Complies c)
        => Profile n            -- ^ Fragment profile giving the supported
                                --   language features and primitive operators.
        -> c a n                -- ^ The thing to check.
        -> Maybe (Error a n)

complies profile thing
 = compliesWithEnvs profile
        (profilePrimKinds profile)
        (profilePrimTypes profile)
        thing


-- | Like `complies` but with some starting environments.
compliesWithEnvs
        :: (Ord n, Show n, Complies c)
        => Profile n            -- ^ Fragment profile giving the supported
                                --   language features and primitive operators.
	-> Env.KindEnv n        -- ^ Starting kind environment.
	-> Env.TypeEnv n        -- ^ Starting type environment.
	-> c a n                -- ^ The thing to check.
	-> Maybe (Error a n)

compliesWithEnvs profile kenv tenv thing
 = let  merr    = result 
                $ compliesX profile 
                        kenv tenv
                        contextTop thing
   in   case merr of
         Left err -> Just err
         Right _  -> Nothing



-- Complies -------------------------------------------------------------------
-- | Class of things we can check language fragment compliance for.
class Complies (c :: * -> * -> *) where
 -- Check compliance of a well typed term with a language profile.
 -- If it is not well typed then this can return a bad result.
 compliesX
        :: (Ord n, Show n)
        => Profile n            -- ^ Fragment profile giving the supported
                                --   language features and primitive operators.
        -> Env n                -- ^ Starting Kind environment.
        -> Env n                -- ^ Starting Type environment.
        -> Context
        -> c a n 
        -> CheckM a n
                (Set n, Set n)  -- Used type and value names.


instance Complies Module where
 compliesX profile kenv tenv context mm
  = do  let bs          = [ BName n t 
                                | (n, (_, t)) <- Map.toList $ moduleImportTypes mm ]
        let tenv'       = Env.extends bs tenv
        compliesX profile kenv tenv' context (moduleBody mm)


-- We'll mark type vars that only appear in types of binders as unused.
instance Complies Exp where
 compliesX profile kenv tenv context xx
  = let has f   = f $ profileFeatures profile
        ok      = return (Set.empty, Set.empty)
    in case xx of

        -- variables ----------------------------
        XVar _ u@(UName n)
         |  not $ Env.member u tenv
         ,  not $ has featuresUnboundLevel0Vars 
         -> throw $ ErrorUndefinedVar n

         |  args        <- fromMaybe 0 $ contextFunArgs context
         ,  Just t      <- Env.lookup u tenv
         ,  arity       <- arityOfType t
         ,  args < arity
         ,  not $ has featuresPartialApplication
         -> throw $ ErrorUnsupported PartialApplication

         | otherwise
         ->     return (Set.empty, Set.singleton n)

        XVar _ u@(UPrim n t)
         |  not $ Env.member u (profilePrimTypes profile)
         -> throw $ ErrorUndefinedPrim n

         |  args        <- fromMaybe 0 $ contextFunArgs context
         ,  arity       <- arityOfType t
         ,  args < arity
         ,  not $ has featuresPartialPrims
         -> throw $ ErrorUnsupported PartialPrims

         | otherwise
         -> return (Set.empty, Set.empty)

        XVar{}          -> ok

        -- constructors -------------------------
        XCon{}          -> ok

        -- spec binders -------------------------
        XLAM _ b x
         | contextAbsBody context
         , not $ has featuresNestedFunctions
         -> throw $ ErrorUnsupported NestedFunctions

         | otherwise
         -> do  
                -- If the body isn't another lambda then remember
                -- that we've entered into a function.
                let context'
                     | isXLAM x || isXLam x = context
                     | otherwise            = setBody context

                (tUsed, vUsed)  <- compliesX profile 
                                        (Env.extend b kenv) tenv 
                                        context' x

                tUsed'          <- checkBind profile kenv b tUsed
                return (tUsed', vUsed)

        -- value and witness abstraction --------
        XLam _ b x
         | contextAbsBody context
         , not $ has featuresNestedFunctions
         -> throw $ ErrorUnsupported NestedFunctions

         | otherwise
         -> do  
                -- If the body isn't another lambda then remember
                -- that we've entered into a function.
                let context'
                     | isXLAM x || isXLam x = context
                     | otherwise            = setBody context

                (tUsed, vUsed)  <- compliesX profile 
                                        kenv (Env.extend b tenv)
                                        context' x

                vUsed'          <- checkBind profile tenv b vUsed
                return (tUsed, vUsed')
       
        -- application --------------------------
        XApp _ x1 (XType _ t2)
         | profileTypeIsUnboxed profile t2
         , Nothing      <- takeXPrimApps xx
         -> throw $ ErrorUnsupported UnboxedInstantiation

         | otherwise
         -> do  checkFunction profile x1
                compliesX     profile kenv tenv (addArg context) x1

        XApp _ x1 XWitness{}
         -> do  checkFunction profile x1
                compliesX     profile kenv tenv (addArg context) x1

        XApp _ x1 x2
         -> do  checkFunction profile x1
                (tUsed1, vUsed1) <- compliesX profile kenv tenv (addArg context) x1
                (tUsed2, vUsed2) <- compliesX profile kenv tenv context x2
                return  ( Set.union tUsed1 tUsed2
                        , Set.union vUsed1 vUsed2)

        -- let ----------------------------------
        XLet _ (LLet b1 x1) x2
         -> do  let tenv'        = Env.extend b1 tenv
                (tUsed1, vUsed1) <- compliesX profile kenv tenv  (reset context) x1
                (tUsed2, vUsed2) <- compliesX profile kenv tenv' (reset context) x2
                vUsed2'          <- checkBind profile tenv b1 vUsed2

                return  ( Set.union tUsed1 tUsed2
                        , Set.union vUsed1 vUsed2')

        XLet _ (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                let tenv'       = Env.extends bs tenv

                (tUseds1, vUseds1) 
                 <- liftM unzip
                 $  mapM (compliesX profile kenv tenv' (reset context)) 
                         xs

                (tUsed2,  vUsed2) <- compliesX profile kenv tenv' (reset context) x2
                let tUseds        = Set.unions (tUsed2 : tUseds1)
                let vUseds        = Set.unions (vUsed2 : vUseds1)

                vUseds'           <- checkBinds profile tenv bs vUseds
                return (tUseds, vUseds')


        XLet _ (LLetRegions rs bs) x2
         -> do  (tUsed2, vUsed2) 
                 <- compliesX profile   (Env.extends rs  kenv) 
                                        (Env.extends bs tenv) 
                                        (reset context) x2
                return (tUsed2, vUsed2)

        XLet _ (LWithRegion _) x2
         -> do  (tUsed2, vUsed2) <- compliesX profile kenv tenv 
                                        (reset context) x2
                return (tUsed2, vUsed2)

        -- case ---------------------------------
        XCase _ x1 alts
         -> do  (tUsed1,  vUsed1)  
                 <- compliesX profile kenv tenv (reset context) x1

                (tUseds2, vUseds2) <- liftM unzip 
                                   $  mapM (compliesX profile kenv tenv (reset context)) alts

                return  ( Set.unions $ tUsed1 : tUseds2
                        , Set.unions $ vUsed1 : vUseds2)


        -- cast ---------------------------------
        XCast _ _ x     -> compliesX profile kenv tenv (reset context) x

        -- type and witness ---------------------
        XType    _ t    -> throw $ ErrorNakedType    t
        XWitness _ w    -> throw $ ErrorNakedWitness w


instance Complies Alt where
 compliesX profile kenv tenv context aa
  = case aa of
        AAlt PDefault x
         -> do  (tUsed1, vUsed1)  <- compliesX profile kenv tenv 
                                        (reset context) x
                return  (tUsed1, vUsed1)

        AAlt (PData _ bs) x
         -> do  (tUsed1, vUsed1) <- compliesX profile kenv (Env.extends bs tenv) 
                                        (reset context) x
                vUsed1'          <- checkBinds profile tenv bs vUsed1 
                return (tUsed1, vUsed1')


-- Bind -----------------------------------------------------------------------
-- | Check for compliance violations at a binding site.
checkBind 
        :: Ord n 
        => Profile n            -- ^ The current language profile.
        -> Env n                -- ^ The current environment
        -> Bind n               -- ^ The binder at this site.
        -> Set n                -- ^ Names used under the binder.
        -> CheckM a n (Set n)   -- ^ Names used above the binder.

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

        BAnon{}
         | not $ has featuresDebruijnBinders
         -> throw $ ErrorUnsupported DebruijnBinders

        _ -> return used


-- | Check for compliance violations at a binding site.
--   The binders must all be at the same level.
checkBinds 
        :: Ord n  
        => Profile n 
        -> Env n  -> [Bind n] -> Set n 
        -> CheckM a n (Set n)

checkBinds profile env bs used
 = case bs of
        []              -> return used
        (b : bs')        
         -> do  used'   <- checkBinds profile env bs' used
                checkBind profile env b used'


-- Function -------------------------------------------------------------------
-- | Check the function part of an application.
checkFunction :: Profile n -> Exp a n -> CheckM a n ()
checkFunction profile xx 
 = let  has f   = f $ profileFeatures profile
        ok       = return ()
   in case xx of
        XVar{}  -> ok
        XCon{}  -> ok
        XApp{}  -> ok
        XCast{} -> ok
        _
         | has featuresGeneralApplication -> return ()
         | otherwise    -> throw $ ErrorUnsupported GeneralApplication


-- Context --------------------------------------------------------------------
data Context
        = Context
        { contextAbsBody        :: Bool 
        , contextFunArgs        :: Maybe Int }
        deriving (Eq, Show)


-- | The top level context, used at the top-level scope of a module.
contextTop :: Context
contextTop
        = Context
        { contextAbsBody        = False
        , contextFunArgs        = Nothing }


-- | Record that we've entered into an abstraction body.
setBody :: Context -> Context
setBody context = context { contextAbsBody = True }


-- | Record that the expression is being directly applied to an argument.
addArg  :: Context -> Context
addArg context
 = case contextFunArgs context of
        Nothing         -> context { contextFunArgs = Just 1 }
        Just args       -> context { contextFunArgs = Just (args + 1) }


-- | Reset the argument counter of a context.
reset   :: Context -> Context
reset context   = context { contextFunArgs = Nothing } 


-- Monad ----------------------------------------------------------------------
-- | Compliance checking monad.
data CheckM a n x
        = CheckM (Either (Error a n) x)

instance Monad (CheckM a n) where
 return x   = CheckM (Right x)
 (>>=) m f  
  = case m of
          CheckM (Left err)     -> CheckM (Left err)
          CheckM (Right x)      -> f x


-- | Throw an error in the monad.
throw :: Error a n -> CheckM a n x
throw e       = CheckM $ Left e


-- | Take the result from a check monad.
result :: CheckM a n x -> Either (Error a n) x
result (CheckM r)       = r
