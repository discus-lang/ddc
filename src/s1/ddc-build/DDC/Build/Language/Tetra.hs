
-- | The `Tetra` fragment has four base kinds:
--   `Data`, `Region`, `Effect`, `Witness`,
--   and uses the `S` computation type to represent effects.
module DDC.Build.Language.Tetra
        ( language
        , bundle
        , fragment)
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment                hiding (Error(..))
import qualified DDC.Core.Tetra         as E
import qualified Data.Map               as Map


-- | Language definition for Disciple Core Tetra.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Tetra.
bundle  :: Bundle Int E.Name E.Error
bundle
        = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier (E.freshT "t")
        , bundleMakeNamifierX   = makeNamifier (E.freshX "x")
        , bundleRewriteRules    = Map.empty }


-- | Fragement definition for Disciple Core Tetra.
fragment :: Fragment E.Name E.Error
fragment
        = Fragment
        { fragmentProfile       = E.profile
        , fragmentExtension     = "dct"
        , fragmentReadName      = E.readName
        , fragmentLexModule     = E.lexModuleString
        , fragmentLexExp        = E.lexExpString
        , fragmentCheckModule   = E.checkModule
        , fragmentCheckExp      = const Nothing }
