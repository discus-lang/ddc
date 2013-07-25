
-- | The `Tetra` fragment has four base kinds: 
--   `Data`, `Region`, `Effect`, `Witness` and uses the `S`
--   computation type to represent effects.
module DDC.Build.Language.Tetra
        ( language
        , bundle
        , fragment

        , Error (..))
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment                hiding (Error(..))
import DDC.Core.Tetra                   as Tetra
import DDC.Core.Tetra.Profile           as Tetra
import DDC.Base.Pretty
import qualified Data.Map               as Map


-- | Language definition for Disciple Core Tetra.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Tetra.
bundle  :: Bundle Int Name Error
bundle
        = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT 
        , bundleMakeNamifierX   = makeNamifier freshX 
        , bundleRewriteRules    = Map.empty }


-- | Fragement definition for Disciple Core Tetra.
fragment :: Fragment Name Error
fragment
        = Fragment
        { fragmentProfile       = profile 
        , fragmentExtension     = "dct"
        , fragmentReadName      = readName
        , fragmentLexModule     = lexModuleString
        , fragmentLexExp        = lexExpString
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


data Error a
        = Error
        deriving Show

instance Pretty (Error a) where
 ppr Error  = text (show Error)

