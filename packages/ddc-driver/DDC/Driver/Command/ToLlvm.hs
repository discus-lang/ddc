
module DDC.Driver.Command.ToLlvm
        (cmdToLlvm)
where
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Driver.Bundle
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import System.FilePath
import System.Exit
import System.IO
import Control.Monad
import qualified DDC.Base.Pretty        as P


-- | Parse, check and convert a  module to LLVM.
--
--   The output is printed to @stdout@. 
--
cmdToLlvm 
        :: Config       -- ^ Compiler configuration.
        -> Bundle       -- ^ Language bundle.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> IO ()

cmdToLlvm config bundle source sourceText
 | Bundle fragment _ _ _ _ <- bundle
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Compile a Core Lite module.
                | fragName == "Lite" || mSuffix == Just ".dcl"
                = pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  config source
                [ stageSaltToLLVM  config source 
                [ PipeLlvmPrint SinkStdout]]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentSalt
                [ stageSaltToLLVM  config source
                [ PipeLlvmPrint SinkStdout]]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to convert this to LLVM"


        -- Print any errors that arose during compilation
        errs <- compile
        mapM_ (hPutStrLn stderr . P.renderIndent . P.ppr) errs

        -- If there were errors then quit and set the exit code.
        when (not $ null errs)
         $ exitWith (ExitFailure 1)
