
module DDC.Driver.Command.Compile
        (cmdCompile)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Pretty        as P


-- | Compile a source module into a @.o@ file.
cmdCompile :: Config -> FilePath -> ErrorT String IO ()
cmdCompile config filePath
 = do   
        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        src             <- liftIO $ readFile filePath
        let ext         = takeExtension filePath
        let source      = SourceFile filePath

        -- Decide what to do based on file extension.
        let make
                -- Compile a Source Tetra module.
                | ext == ".dst"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSourceTetraLoad config source
                [ PipeCoreReannotate  (const ())
                [ stageTetraToSalt     config source pipesSalt ]]

                -- Compile a Core Tetra module.
                | ext == ".dct"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageTetraLoad    config source
                [ stageTetraToSalt  config source pipesSalt ]

                -- Compile a Core Lite module.
                | ext == ".dcl"
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageLiteLoad     config source
                [ stageLiteOpt      config source  
                [ stageLiteToSalt   config source pipesSalt ]]

                -- Compile a Core Salt module.
                | ext == ".dcs"
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSaltLoad     config source pipesSalt

                -- Unrecognised.
                | otherwise
                = throwError $ "Cannot compile '" ++ ext ++ "' files."

            pipesSalt
             = case configViaBackend config of
                ViaLLVM
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt      config source
                    [ stageSaltToLLVM   config source 
                    [ stageCompileLLVM  config source filePath False ]]]]

                ViaC
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt      config source
                    [ stageCompileSalt  config source filePath False ]]]

        -- Throw any errors that arose during compilation.
        errs <- make
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

