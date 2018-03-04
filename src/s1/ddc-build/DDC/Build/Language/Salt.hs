
-- | The `Salt` fragment contains just those features that can be easily mapped
--   onto C or LLVM code.
module DDC.Build.Language.Salt
        ( language
        , bundle
        , fragment)
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment
import DDC.Core.Salt                    as Salt
import qualified DDC.Core.Salt          as A
import qualified Data.Map               as Map
import qualified Data.Text              as T


-- | Language definition for Disciple Core Salt.
language :: Language
language = Language bundle


-- | Language bundle for Disciple Core Salt.
bundle   :: Bundle Int A.Name A.Error
bundle  = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT
        , bundleMakeNamifierX   = makeNamifier freshX
        , bundleRewriteRules    = Map.empty }


-- | Fragment definition for Disciple Core Salt.
fragment :: Fragment A.Name A.Error
fragment
        = Fragment
        { fragmentProfile       = A.profile
        , fragmentExtension     = "dcs"
        , fragmentReadName      = A.readName . T.pack
        , fragmentLexModule     = A.lexModuleString
        , fragmentLexExp        = A.lexExpString
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }

