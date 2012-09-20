
module DDC.Main.Bundle
        (Bundle (..))
where
import DDC.Build.Language
import DDC.Core.Simplifier
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Check
import DDC.Core.Module
import DDC.Base.Pretty
import Data.Typeable
import Data.Map                         (Map)


-- | Existential container for a language fragment, 
--      the simplifier for it,
--      and the dictionaries we need to work with its type parameters.
data Bundle
        = forall s n err
        .  (Typeable n, Ord n, Show n, Pretty n, Pretty (err (AnTEC () n)))
        => Bundle 
        {  bundleFragment        :: Fragment n err
        ,  bundleModules         :: Map ModuleName (Module (AnTEC () n) n)
        ,  bundleStateInit       :: s
        ,  bundleSimplifier      :: Simplifier s (AnTEC () n) n
        ,  bundleRewriteRules    :: Map String (RewriteRule (AnTEC () n) n) }
