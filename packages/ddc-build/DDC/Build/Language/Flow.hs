
-- | The `Flow` fragment is used for data-flow optimisation as part
--   of the Data Parallel Haskell vectorisation pipeline.
module DDC.Build.Language.Flow
        ( language
        , bundle
        , fragment

        , Error (..))
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment                hiding (Error(..))
import DDC.Core.Flow                    as Flow
import DDC.Core.Flow.Profile            as Flow
import DDC.Base.Pretty
import qualified Data.Map               as Map


-- | Language definition for Disciple Core Lite.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Lite.
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


-- | Fragement definition for Disciple Core Lite.
fragment :: Fragment Name Error
fragment
        = Fragment
        { fragmentProfile       = profile 
        , fragmentExtension     = "dcf"
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

