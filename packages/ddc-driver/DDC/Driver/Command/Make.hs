
module DDC.Driver.Command.Make
        (cmdMake)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import System.FilePath
import System.Directory
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Control.Monad
import qualified DDC.Core.Pretty        as P


-- | Make a source module into an executable.
cmdMake :: Config -> FilePath -> ErrorT String IO ()
cmdMake config filePath
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
                -- Make a Source Tetra module.
                | ext == ".dst"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSourceTetraLoad config source []
                [ PipeCoreReannotate  (const ())
                [ stageTetraToSalt     config source pipesSalt ]]

                -- Make a Core Tetra module.
                | ext == ".dct"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageTetraLoad    config source
                [ stageTetraToSalt  config source pipesSalt ]

                -- Make a Core Lite module.
                | ext == ".dcl"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageLiteLoad     config source
                [ stageLiteOpt      config source  
                [ stageLiteToSalt   config source pipesSalt ]]

                -- Make a Core Salt module.
                | ext == ".dcs"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSaltLoad     config source pipesSalt 

                -- Unrecognised.
                | otherwise
                = throwError $ "Cannot make '" ++ filePath ++ "' files."

            pipesSalt
             = case configViaBackend config of
                ViaLLVM
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt      config source
                    [ stageSaltToLLVM   config source 
                    [ stageCompileLLVM  config source filePath True ]]]]

                ViaC
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt      config source
                    [ stageCompileSalt  config source filePath True ]]]

        -- Throw any errors that arose during compilation.
        errs <- make
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es
