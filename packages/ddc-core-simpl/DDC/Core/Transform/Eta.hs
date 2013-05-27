
-- Eta-expand functional values.
---
-- NOTE: This is module currently just does eta-expansion, but in future
--       we should expand the config to also make it do expansion/contraction
--       based on the real arity of bindings.
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
-- import Debug.Trace

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
-- | Eta-transform functional values in a module.
etaModule
        :: (Ord n, Show n, Pretty n, Show a)
        => Config
        -> Profile n
        -> Module a n
        -> TransformResult (Module a n)

etaModule config profile mm
 = let  cconfig     = Check.configOfProfile profile
        (mm', info) = runWriter
                    $ etaModuleM config cconfig
                        (profilePrimKinds profile)
                        (profilePrimTypes profile) 
                        mm

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

etaModuleM
        :: (Ord n, Show n, Pretty n, Show a)
        => Config -> Check.Config n
        -> KindEnv n
        -> TypeEnv n
        -> Module a n
        -> Writer Info (Module a n)

etaModuleM config cconfig kenv tenv mm
 = do   let kenv'       = Env.union (moduleKindEnv mm) kenv
        let tenv'       = Env.union (moduleTypeEnv mm) tenv
        xx'             <- etaXM config cconfig kenv' tenv' (moduleBody mm)
        return  $ mm { moduleBody = xx' }


-- Exp ------------------------------------------------------------------------
-- | Eta-transform functional values in an expression.
etaX    :: (Ord n, Show n, Show a, Pretty n)
        => Config -> Profile n
        -> KindEnv n -> TypeEnv n
        -> Exp a n
        -> TransformResult (Exp a n)

etaX config profile kenv tenv xx
 = let  cconfig         = Check.configOfProfile profile
        (xx', info)     = runWriter
                        $ etaXM config cconfig
                                (Env.union (profilePrimKinds profile) kenv)
                                (Env.union (profilePrimTypes profile) tenv)
                                xx

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


etaXM   :: (Ord n, Show n, Show a, Pretty n)
        => Config -> Check.Config n
        -> KindEnv n -> TypeEnv n
        -> Exp a n
        -> Writer Info (Exp a n)

etaXM config cconfig kenv tenv xx
 = case xx of
        XApp a _ _
         | configExpand config
         , Right tX     <- Check.typeOfExp cconfig kenv tenv xx
         -> etaExpand a tX xx

        _ -> return xx


-- Expand ---------------------------------------------------------------------
-- | Eta expand an expression.
etaExpand 
        :: (Ord n, Pretty n)
        => a                    -- ^ Annotation to use for new AST nodes.
        -> Type n               -- ^ Type of the expression.
        -> Exp a n              -- ^ Inner expression to wrap.
        -> Writer Info (Exp a n)

etaExpand a tX xx
 = do   let btsMore     = expandableArgs tX
        xx'             <- etaExpand' a 0 0 [] btsMore xx
{-        trace (renderIndent
              $ vcat [ text "EtaExpand"
                     , ppr tX
                     , ppr btsMore
                     , text "before: " <> ppr xx
                     , text "after:  " <> ppr xx'
                     , text ""]) $ -}
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

