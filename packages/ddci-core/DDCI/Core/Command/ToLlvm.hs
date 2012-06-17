
module DDCI.Core.Command.ToLlvm
        (cmdToLlvm)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Pipeline
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Check
import System.FilePath
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Core.Salt          as A
import qualified DDC.Base.Pretty        as P
import Data.Monoid

-- | Parse, check and convert a  module to LLVM.
---
--   The Brine -> C conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdToLlvm :: State -> Source -> String -> IO ()
cmdToLlvm state source str
 | Language fragment    <- stateLanguage state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        builder         <- getActiveBuilder state

        if      fragName == "Salt"  || mSuffix == Just ".dce"
         then cmdSaltToLlvm state source str builder
        else if fragName == "Lite"  || mSuffix == Just ".dcl"
         then cmdLiteToLlvm  state source str builder
        else error $ "Don't know how to convert Disciple " ++ fragName ++ " module to C code."


-- | Convert a Disciple Lite module to Llvm code.
cmdLiteToLlvm :: State -> Source -> String -> Builder -> IO ()
cmdLiteToLlvm state source str builder
 = (pipeText (nameOfSource source) (lineStartOfSource source) str
        $  PipeTextLoadCore     fragmentLite
        [  PipeCoreAsLite
        [  PipeLiteToSalt       (buildSpec builder)
        [  PipeCoreSimplify     fragmentSalt Simpl.anormalize
        [  PipeCoreCheck        fragmentSalt 
        [  pipeCore_saltToLlvm state builder]]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


-- | Convert a Disciple Salt module to Llvm code.
cmdSaltToLlvm :: State -> Source -> String -> Builder -> IO ()
cmdSaltToLlvm state source str builder
 = (pipeText (nameOfSource source) (lineStartOfSource source) str
        $  PipeTextLoadCore     fragmentSalt
        [  pipeCore_saltToLlvm state builder])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


pipeCore_saltToLlvm :: Show a => State -> Builder -> PipeCore (AnTEC a A.Name) A.Name
pipeCore_saltToLlvm state builder
        =  PipeCoreSimplify    fragmentSalt
                               (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreReCheck     fragmentSalt
        [  PipeCoreAsSalt
        [  PipeSaltToLlvm      (buildSpec builder)
        [  PipeLlvmPrint SinkStdout ]]]]
