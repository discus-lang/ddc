
-- | Common simplifier recipes that combine multiple transforms.
module DDC.Core.Simplifier.Recipe
        ( -- * Atomic recipies
          idsimp
        , anonymize
        , snip
        , snipOver
        , flatten
        , beta
        , betaLets
        , prune
        , forward
        , bubble
        , elaborate

          -- * Compound recipies
        , anormalize
        , rewriteSimp)
where
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.Namify
import qualified DDC.Core.Transform.Snip  as Snip
import qualified DDC.Core.Transform.Beta  as Beta
import DDC.Type.Env
import Data.Monoid


-- Atomic ---------------------------------------------------------------------
-- These are short names for single transforms.

-- | The identity simplifier returns the code unharmed.
idsimp    :: Simplifier s a n
idsimp    = Trans Id


-- | Rewrite named binders to anonymous debruijn binders.
anonymize :: Simplifier s a n
anonymize = Trans Anonymize


-- | Introduce let-bindings for nested applications.
snip      :: Simplifier s a n
snip      = Trans (Snip Snip.configZero)


-- | Introduce let-bindings for nested applications.
snipOver  :: Simplifier s a n
snipOver  = Trans (Snip Snip.configZero { Snip.configSnipOverApplied = True })


-- | Flatten nested let and case expressions.
flatten   :: Simplifier s a n
flatten   = Trans Flatten


-- | Perform beta reduction
beta    :: Simplifier s a n
beta    = Trans (Beta Beta.configZero)


-- | Perform beta reduction, introducing let-expressions for compound arguments.
betaLets :: Simplifier s a n
betaLets = Trans (Beta Beta.configZero { Beta.configBindRedexes = True })


-- | Remove unused, pure let bindings.
prune  :: Simplifier s a n
prune   = Trans Prune


-- | Float single-use bindings forward into their use sites.
forward  :: Simplifier s a n
forward  = Trans Forward


-- | Float casts outwards.
bubble   :: Simplifier s a n
bubble   = Trans Bubble


-- | Elaborate possible Const and Distinct witnesses that aren't
--   otherwise in the program.
elaborate :: Simplifier s a n
elaborate = Trans Elaborate


-- Compound -------------------------------------------------------------------
-- | Conversion to administrative normal-form.
anormalize 
        :: (KindEnv n -> Namifier s n) 
                -- ^ Make a namifier to create fresh level-1 names.
        -> (TypeEnv n -> Namifier s n) 
                -- ^ Make a namifier to create fresh level-0 names.
        -> Simplifier s a n

anormalize namK namT
        =  snip
        <> Trans Flatten 
        <> Trans (Namify namK namT)


-- | Intersperse rewrites and beta reduction
rewriteSimp
        :: Int                          -- ^ Maximum number of iterations.
        -> NamedRewriteRules a n        -- ^ Rewrite rules to apply.
        -> Simplifier s a n

rewriteSimp maxIters rules
 = let  rewrite = Trans $ Rewrite rules
   in   Fix maxIters (rewrite <> bubble <> betaLets)

