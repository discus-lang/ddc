
module DDC.Driver.Command.ToLlvm
        ( cmdToLlvmFromFile
        , cmdToLlvmSourceTetraFromFile
        , cmdToLlvmSourceTetraFromString
        , cmdToLlvmCoreFromFile
        , cmdToLlvmCoreFromString)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Base.Pretty
import System.Directory
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Build.Interface.Store        (Store)


-------------------------------------------------------------------------------
-- | Convert a module to LLVM.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToLlvmFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Core language definition.
        -> ExceptT String IO ()

cmdToLlvmFromFile config store filePath
 
 -- Convert a Disciple Source Tetra module.
 | ".ds"          <- takeExtension filePath
 =      cmdToLlvmSourceTetraFromFile config store filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language  <- languageOfExtension (takeExtension filePath)
 =      cmdToLlvmCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to LLVM."


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to LLVM.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToLlvmSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToLlvmSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToLlvmSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to LLVM.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToLlvmSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToLlvmSourceTetraFromString config store source str
 = let  
        pipeLoad
         = pipeText (nameOfSource source) (lineStartOfSource source) str
         $ stageSourceTetraLoad config source store
         [ PipeCoreReannotate (const ())
         [ stageTetraToSalt     config source 
         [ stageSaltOpt         config source
         [ stageSaltToLLVM      config source
         [ PipeLlvmPrint SinkStdout]]]]]
   
   in do
        errs    <- liftIO pipeLoad
        case errs of
         []     -> return ()
         es     -> throwE $ renderIndent $ vcat $ map ppr es


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to LLVM.
--   Works for the 'Tetra', 'Lite' and 'Salt' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToLlvmCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToLlvmCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToLlvmCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to LLVM.
--   Works for the 'Tetra', 'Lite' and 'Salt' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToLlvmCoreFromString
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToLlvmCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = do   
        -- Language fragment name.
        let fragName = profileName profile
        
        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Convert a Core Tetra module to LLVM.
                | fragName == "Tetra"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageTetraLoad     config source
                [ stageTetraToSalt   config source
                [ stageSaltOpt       config source
                [ stageSaltToLLVM    config source
                [ PipeLlvmPrint      SinkStdout]]]]

                -- Convert a Core Lite module to LLVM.
                | fragName == "Lite"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageLiteLoad      config source
                [ stageLiteOpt       config source
                [ stageLiteToSalt    config source
                [ stageSaltOpt       config source
                [ stageSaltToLLVM    config source 
                [ PipeLlvmPrint SinkStdout]]]]]

                -- Convert a Core Salt module to LLVM.
                | fragName == "Salt"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageSaltLoad      config source
                [ stageSaltOpt       config source
                [ stageSaltToLLVM    config source
                [ PipeLlvmPrint      SinkStdout]]]

                -- Unrecognised.
                | otherwise
                = throwE $ "Cannot convert '" ++ fragName ++ "' modules to LLVM."

        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwE $ renderIndent $ vcat $ map ppr es
