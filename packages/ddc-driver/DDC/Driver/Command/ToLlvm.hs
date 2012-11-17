
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
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
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
        -> ErrorT String IO ()

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
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  config source
                [ stageSaltToLLVM  config source 
                [ PipeLlvmPrint SinkStdout]]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentSalt
                [ stageSaltToLLVM  config source
                [ PipeLlvmPrint SinkStdout]]

                -- Unrecognised.
                | otherwise
                = throwError $ "Don't know how to convert this to LLVM"

        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es
