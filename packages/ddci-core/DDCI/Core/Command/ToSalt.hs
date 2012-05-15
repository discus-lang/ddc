
module DDCI.Core.Command.ToSalt
        (cmdToSalt)
where
import DDCI.Core.Interface.Suppress
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Data.Canned
import System.FilePath
import DDC.Core.Simplifier                      as Simpl
import qualified DDC.Base.Pretty                as P


-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> Salt conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdToSalt :: State -> Source -> String -> IO ()
cmdToSalt state source str
 | Language fragment    <- stateLanguage state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        builder         <- getActiveBuilder state

        if fragName == "Lite"  || mSuffix == Just ".dcl"
         then cmdLiteToSalt  state source builder str
        else error $ "Don't know how to convert Disciple " ++ fragName ++ " module to Disciple Salt."


-- | Convert a Disciple Lite module to C code.
cmdLiteToSalt :: State -> Source -> Builder -> String -> IO ()
cmdLiteToSalt state source builder str
 = (pipeText (nameOfSource source) (lineStartOfSource source) str
        $  PipeTextLoadCore     fragmentLite
        [  PipeCoreSimplify     fragmentLite Simpl.anormalize
        [  PipeCoreReCheck      fragmentLite 
        [  PipeCoreAsLite
        [  PipeLiteToSalt       (buildSpec builder)
        [  PipeCoreSimplify     fragmentSalt Simpl.anormalize
        [  PipeCoreCheck        fragmentSalt
        [  PipeCoreHacks        (Canned (suppressModule state))
        [  PipeCoreOutput       SinkStdout ]]]]]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)
