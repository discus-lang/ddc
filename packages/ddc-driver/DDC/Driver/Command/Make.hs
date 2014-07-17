

-- TODO: Deduplicate this code. 
--       Most is copy-pasted from the Compile command.
--
module DDC.Driver.Command.Make
        (cmdMake)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Interface.Base
import DDC.Data.Canned
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.IORef
import qualified DDC.Core.Pretty        as P
import qualified DDC.Core.Module        as C
import qualified DDC.Version            as Version


-- | Make a source module into an executable.
cmdMake :: Config               -- ^ Build driver config.
        -> [InterfaceAA]        -- ^ Interfaces of modules that we've already loaded.
        -> FilePath             -- ^ Path to file to compile.
        -> ErrorT String IO ()

cmdMake config interfaces filePath
 = do
        -- Paths of other object files to link with
        let pathOtherObjs   
                = map (\i -> replaceExtension (interfaceFilePath i) "o")
                $ interfaces

        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        src             <- liftIO $ readFile filePath
        let ext         = takeExtension filePath
        let source      = SourceFile filePath

        -- During complation, intermediate code will be stashed in these refs.
        refTetra        <- liftIO $ newIORef Nothing
        refSalt         <- liftIO $ newIORef Nothing

        -- Decide what to do based on file extension.
        let make
                -- Make a Source Tetra module.
                | ext == ".ds"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSourceTetraLoad config source interfaces
                [ PipeCoreHacks       (Canned $ \m -> writeIORef refTetra (Just m) >> return m)
                [ PipeCoreReannotate  (const ())
                [ stageTetraToSalt     config source pipesSalt ]]]

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
                    [ PipeCoreHacks    (Canned $ \m -> writeIORef refSalt (Just m) >> return m)
                    [ stageSaltToLLVM   config source 
                    [ stageCompileLLVM  config source filePath (Just pathOtherObjs) ]]]]]

                ViaC
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt      config source
                    [ stageCompileSalt  config source filePath True ]]]

        -- Run the compilation pipeline.
        errs <- make

        -- Read back intermediate code from our refs.
        --   This will be written out as part of the interface file for this module.
        modTetra  <- liftIO $ readIORef refTetra
        modSalt   <- liftIO $ readIORef refSalt

        case errs of
         -- There was some error during compilation.
         es@(_:_)
          -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

         -- Compilation was successful, 
         --  but we need to have produced at least a Tetra or Salt module
         --  before we can build the interface file.
         []    
          | Just (mn : _)       
                <- sequence 
                        [ liftM C.moduleName modTetra
                        , liftM C.moduleName modSalt ]
          -> do
                -- write out the interface file.
                let pathInterface   = replaceExtension filePath "di"
                let int = Interface
                        { interfaceVersion      = Version.version
                        , interfaceFilePath     = pathInterface
                        , interfaceModuleName   = mn
                        , interfaceTetraModule  = modTetra 
                        , interfaceSaltModule   = modSalt }

                liftIO  $ writeFile pathInterface 
                        $ P.renderIndent $ P.ppr int

                return ()

          -- Compilation was successful,
          --  but we didn't get enough build products to produce an interface file.
          | otherwise
          -> return ()




