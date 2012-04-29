
module DDCI.Core.Command.ToLlvm
        (cmdToLlvm)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Core.Fragment.Profile
import System.FilePath
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Core.Brine.Output  as E
import qualified DDC.Base.Pretty        as P
import Data.Monoid
import Data.Maybe

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

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform.")
                                        mBuilder

        if      fragName == "Brine" || mSuffix == Just ".dce"
         then cmdBrineToLlvm state source str builder
        else if fragName == "Lite"  || mSuffix == Just ".dcl"
         then cmdLiteToLlvm  state source str builder
        else error $ "Don't know how to convert Disciple " ++ fragName ++ " module to C code."


-- | Convert a Disciple Lite module to Llvm code.
cmdLiteToLlvm :: State -> Source -> String -> Builder -> IO ()
cmdLiteToLlvm state source str builder
 = (pipeText source str
        $  PipeTextLoadCore     fragmentLite
        [  PipeCoreAsLite
        [  PipeLiteToBrine
        [  pipeCore_brineToLlvm state builder]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


-- | Convert a Disciple Brine module to Llvm code.
cmdBrineToLlvm :: State -> Source -> String -> Builder -> IO ()
cmdBrineToLlvm state source str builder
 = (pipeText source str
        $  PipeTextLoadCore     fragmentBrine
        [  pipeCore_brineToLlvm state builder])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


pipeCore_brineToLlvm :: State -> Builder -> PipeCore E.Name
pipeCore_brineToLlvm state builder
        = PipeCoreSimplify     fragmentBrine   
                               (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreCheck       fragmentBrine
        [  PipeCoreAsBrine
        [  PipeBrineToLlvm      (buildSpec builder)
        [  PipeLlvmPrint SinkStdout ]]]]
