
module DDCI.Core.Command.ToLlvm
        (cmdToLlvm)
where
import DDCI.Core.State
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import System.FilePath
import qualified DDC.Base.Pretty        as P


-- | Parse, check and convert a  module to LLVM.
---
cmdToLlvm :: State -> Source -> String -> IO ()
cmdToLlvm state source str
 | Bundle fragment _ _ _ _ <- stateBundle state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Slurp out the driver config we need from the DDCI state.
        config          <- getDriverConfigOfState state

        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Compile a Core Lite module.
                | fragName == "Lite" || mSuffix == Just ".dcl"
                = pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  config source
                [ stageSaltToLLVM  config source 
                [ PipeLlvmPrint SinkStdout]]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore fragmentSalt
                [ stageSaltToLLVM  config source
                [ PipeLlvmPrint SinkStdout]]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to convert this to LLVM"


        -- Print any errors that arose during compilation
        errs <- compile

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
