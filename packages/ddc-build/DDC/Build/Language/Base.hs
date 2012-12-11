
module DDC.Build.Language.Base
        ( Language (..)
        , Bundle   (..))
where
import DDC.Core.Module
import DDC.Core.Fragment
import DDC.Core.Check
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Typeable
import Data.Map                         (Map)
import DDC.Type.Env                     (Env)


-- | Existential container for a language fragment, and the dictionaries
--   we need to work with its type parameters.
data Language
        = forall s n err
        . ( Typeable n
          , Ord n
          , Show n
          , Pretty n
          , Pretty (err (AnTEC () n))
          , NFData n)
        => Language (Bundle s n err)


-- | Existential container for a language fragment, 
--      the simplifier for it,
--      and the dictionaries we need to work with its type parameters.
data Bundle s n err
        =  Bundle 
        {  -- | Language fragment definition.
           bundleFragment        :: Fragment n err

           -- | Modules being used for inliner templates.
        ,  bundleModules         :: Map ModuleName (Module (AnTEC () n) n)

           -- | Initial simplifier state.
        ,  bundleStateInit       :: s

           -- | Current simplifier to apply to module.
        ,  bundleSimplifier      :: Simplifier s (AnTEC () n) n

           -- | Make a namifier for level-1 names.
        ,  bundleMakeNamifierT   :: Env n -> Namifier s n

           -- | Make a namifier for level-0 names.
        ,  bundleMakeNamifierX   :: Env n -> Namifier s n

           -- | Current rewrite rules to apply to module.
        ,  bundleRewriteRules    :: Map String (RewriteRule (AnTEC () n) n) }

