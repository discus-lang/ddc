
module DDC.Driver.Command.Compile
        (cmdCompile)
where
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import System.Directory
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.List
import qualified DDC.Core.Pretty        as P


-- | Compile a source module into a @.o@ file.
-- 
cmdCompile :: Config -> FilePath -> ErrorT String IO ()
cmdCompile config filePath
 = do   
        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        src             <- liftIO $ readFile filePath
        let source      = SourceFile filePath

        -- Decide what to do based on file extension.
        let make
                -- Make a Core Lite module.
                | isSuffixOf ".dcl" filePath
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageLiteLoad     config source
                [ stageLiteOpt      config source  
                [ stageLiteToSalt   config source pipesSalt ]]

                -- Make a Core Salt module.
                | isSuffixOf ".dcs" filePath
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ PipeTextLoadCore  fragmentSalt pipesSalt

                -- Unrecognised.
                | otherwise
                = throwError $ "Don't know how to compile " ++ filePath

            pipesSalt
             = case configViaBackend config of
                ViaLLVM
                 -> [ PipeCoreStrip
                    [ stageSaltOpt      config source
                    [ stageSaltToLLVM   config source 
                    [ stageCompileLLVM  config source filePath False ]]]]

                ViaC
                 -> [ PipeCoreStrip
                    [ stageSaltOpt      config source
                    [ stageCompileSalt  config source filePath False ]]]

        -- Throw any errors that arose during compilation.
        errs <- make
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

