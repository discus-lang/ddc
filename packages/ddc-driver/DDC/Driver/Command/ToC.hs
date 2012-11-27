
module DDC.Driver.Command.ToC
        (cmdToC)
where
import DDC.Driver.Bundle
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import System.FilePath
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty        as P


-- | Parse, check, and convert a module to C.
--
--   The output is printed to @stdout@. 
--
cmdToC  :: Config       -- ^ Compiler configuration.
        -> Bundle       -- ^ Language bundle.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdToC config bundle source sourceText
 | Bundle fragment _ _ _  _ <- bundle
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
                [ PipeCoreStrip
                [ stageLiteToSalt  config source 
                [ stageSaltToC     config source SinkStdout]]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentSalt
                [ PipeCoreStrip
                [ stageSaltToC     config source SinkStdout]]

                -- Unrecognised.
                | otherwise
                = throwError "Don't know how to convert this to C"


        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

