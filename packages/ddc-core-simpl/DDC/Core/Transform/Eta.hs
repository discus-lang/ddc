
-- Eta-expand functional values.
---
-- NOTE: This is module currently just does eta-expansion, but in future
--       we should expand the config to also make it do expansion/contraction
--       based on the real arity of bindings.
--
module DDC.Core.Transform.Eta
        ( Info  (..)
        , Config(..)
        , configZero
        , etaModule
        , etaX)
where
import qualified DDC.Core.Check as Check
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Fragment
import DDC.Core.Transform.LiftX
import DDC.Core.Transform.LiftT
import DDC.Core.Simplifier.Result
import DDC.Core.Compounds
import DDC.Core.Pretty
import DDC.Type.Env             (TypeEnv, KindEnv)
import Control.Monad.Writer     (Writer, tell, runWriter)
import Data.Monoid              (Monoid, mempty, mappend)
import qualified DDC.Type.Env   as Env
import Data.Typeable


-------------------------------------------------------------------------------
data Info
        = Info
        { -- | Number of level-1 lambdas added.
          infoExpandedXLAMs     :: Int 

          -- | Number of level-0 lambdas added. 
        , infoExpandedXLams     :: Int }
        deriving Typeable


instance Pretty Info where
 ppr (Info ex1 ex0)
  = text "Eta Transform"
  <$> indent 4 (vcat
      [ text "level-1 lambdas added:     " <> int ex1 
      , text "level-0 lambdas added:     " <> int ex0 ])


instance Monoid Info where
 mempty  = Info 0 0
 mappend (Info ex1  ex0)
         (Info ex1' ex0')
  = Info (ex1 + ex1') (ex0 + ex0')


-------------------------------------------------------------------------------
data Config
        = Config
        { configExpand  :: Bool }


-- | Empty eta configuration with all flags set to False.
configZero :: Config
configZero
        = Config
        { configExpand  = False }


-- Module ---------------------------------------------------------------------
-- | Eta-transform expressions in a module.
etaModule
        :: (Ord n, Show n, Pretty n, Show a)
        => Config
        -> Profile n
        -> Module a n
        -> TransformResult (Module a n)

etaModule config profile mm
 = let  cconfig = Check.configOfProfile profile
        kenv'   = Env.union (profilePrimKinds profile) (moduleKindEnv mm)
        tenv'   = Env.union (profilePrimTypes profile) (moduleTypeEnv mm)
        
        -- Run the eta transform.
        (mm', info) 
                = runWriter 
                $ etaM config cconfig kenv' tenv' mm
                    
        -- Check if any actual work was performed
        progress
         = case info of
                Info ex1 ex0
                 -> ex1 + ex0 > 0

   in   TransformResult
         { result         = mm'
         , resultAgain    = False
         , resultProgress = progress
         , resultInfo     = TransformInfo info }


-- | Eta-transform an expression.
etaX    :: (Ord n, Show n, Show a, Pretty n)
        => Config               -- ^ Eta-transform config.
        -> Profile n            -- ^ Language profile.
        -> KindEnv n            -- ^ Kind environment.
        -> TypeEnv n            -- ^ Type environment.
        -> Exp a n              -- ^ Expression to transform.
        -> TransformResult (Exp a n)

etaX config profile kenv tenv xx
 = let  cconfig = Check.configOfProfile profile
        kenv'   = Env.union (profilePrimKinds profile) kenv
        tenv'   = Env.union (profilePrimTypes profile) tenv

        -- Run the eta transform.
        (xx', info)     
                = runWriter
                $ etaM config cconfig kenv' tenv' xx

        -- Check if any actual work was performed
        progress
         = case info of
                Info ex1 ex0
                 -> ex1 + ex0 > 0

   in   TransformResult
         { result         = xx'
         , resultAgain    = False
         , resultProgress = progress
         , resultInfo     = TransformInfo info }


-------------------------------------------------------------------------------
class Eta (c :: * -> * -> *) where
 etaM   :: (Ord n, Pretty n, Show n)
        => Config               -- ^ Eta-transform config.
        -> Check.Config n       -- ^ Type checker config.
        -> KindEnv n            -- ^ Kind environment.
        -> TypeEnv n            -- ^ Type environment.
        -> c a n                -- ^ Do eta-expansion in this thing.
        -> Writer Info (c a n)


instance Eta Module where
 etaM config cconfig kenv tenv mm
  = do  let kenv'       = Env.union (moduleKindEnv mm) kenv
        let tenv'       = Env.union (moduleTypeEnv mm) tenv
        xx'             <- etaM config cconfig kenv' tenv' (moduleBody mm)
        return  $ mm { moduleBody = xx' }


instance Eta Exp where
 etaM config cconfig kenv tenv xx
  = let down = etaM config cconfig kenv tenv
    in case xx of

        XVar a _
         | configExpand config
         , Right tX     <- Check.typeOfExp cconfig kenv tenv xx
         -> do  etaExpand a tX xx

        XApp a _ _
         |  configExpand config
         ,  Right tX    <- Check.typeOfExp cconfig kenv tenv xx
         -> do  
                -- Decend into the arguments first.
                --   We don't need to decend into the function part because
                --   we're eta-expanding that here.
                let (x : xs)    =  takeXAppsAsList xx
                xs_eta          <- mapM down xs

                -- Now eta expand the result.
                etaExpand a tX $ xApps a x xs_eta

        XLAM a b x
         -> do  let kenv'       = Env.extend b kenv
                x'              <- etaM config cconfig kenv' tenv x
                return $ XLAM a b x'

        XLam a b x
         -> do  let tenv'       = Env.extend b tenv
                x'              <- etaM config cconfig kenv tenv' x
                return $ XLam a b x'

        XLet a lts x2
         -> do  lts'            <- down lts
                let (bs1, bs0)  = bindsOfLets lts
                let kenv'       = Env.extends bs1 kenv
                let tenv'       = Env.extends bs0 tenv
                x2'             <- etaM config cconfig kenv' tenv' x2
                return $ XLet a lts' x2'

        XCase a x alts
         -> do  x'              <- down x
                alts'           <- mapM (etaM config cconfig kenv tenv) alts
                return $ XCase a x' alts'

        XCast a cc x
         -> do  x'              <- down x
                return $ XCast a cc x'

        _ -> return xx


instance Eta Lets where
 etaM config cconfig kenv tenv lts
  = let down    = etaM config cconfig kenv tenv
    in case lts of
        LLet b x
         -> do  x'      <- down x
                return  $ LLet b x'

        LRec bxs
         -> do  let bs    = map fst bxs
                let tenv' = Env.extends bs tenv
                xs'       <- mapM (etaM config cconfig kenv tenv') 
                          $  map snd bxs
                return    $ LRec (zip bs xs')

        LLetRegions{}   -> return lts
        LWithRegion{}   -> return lts


instance Eta Alt where
 etaM config cconfig kenv tenv alt
  = case alt of
        AAlt p x        
         -> do  let bs    = bindsOfPat p
                let tenv' = Env.extends bs tenv
                x'        <- etaM config cconfig kenv tenv' x
                return  $ AAlt p x'


-- Expand ---------------------------------------------------------------------
-- | Eta expand an expression.
etaExpand 
        :: Ord n
        => a                    -- ^ Annotation to use for new AST nodes.
        -> Type n               -- ^ Type of the expression.
        -> Exp a n              -- ^ Inner expression to wrap.
        -> Writer Info (Exp a n)

etaExpand a tX xx
 = do   let btsMore     = expandableArgs tX
        xx'             <- etaExpand' a 0 0 [] btsMore xx
        return xx'


-- | Decide what type arguments need to be eta-expanded.
expandableArgs :: Type n -> [(Bool, Type n)]
expandableArgs tt
        | TForall b t'          <- tt
        = (True, typeOfBind b)  : expandableArgs t'

        | Just (t1, _, _, t2)   <- takeTFun tt
        = (False, t1)           : expandableArgs t2

        | otherwise
        = []


-- | Eta-expand an expression.
etaExpand'
        :: Ord n 
        => a                -- ^ Annotation to use for the new AST nodes.
        -> Int              -- ^ Number of level-1 lambdas we've added so far.
        -> Int              -- ^ Number of level-0 lambdas we've added so far.
        -> [Exp a n]        -- ^ Accumulate arguments we need to add to the
                            --      inner expression.
        -> [(Bool, Type n)] -- ^ Types of bindings we need to add, along with
                            --   a flag to indicate level-1 or level-0 binder
        -> Exp a n          -- ^ Inner expression that is being applied.
        -> Writer Info (Exp a n)

etaExpand' a levels1 levels0 args [] xx
 = do   let xx' = liftT levels1 $ liftX levels0 xx
        return  $ xApps a xx' args

etaExpand' a levels1 levels0 args ((True, t) : ts) xx
 = do   let depth1 = length $ filter ((== True) . fst) ts
        xx'     <- etaExpand' a (levels1 + 1) levels0 
                        (args ++ [XType (TVar (UIx depth1))]) 
                        ts
                        xx

        tell mempty { infoExpandedXLAMs = 1 }
        return  $ XLAM a (BAnon t) xx'

etaExpand' a levels1 levels0 args ((False, t) : ts) xx
 = do   let depth0 = length $ filter ((== False) . fst) ts
        xx'     <- etaExpand' a 
                        levels1 (levels0 + 1) 
                        (args ++ [XVar a (UIx depth0)])
                        ts
                        xx

        tell mempty { infoExpandedXLams = 1 }
        return  $ XLam a (BAnon t) xx'

