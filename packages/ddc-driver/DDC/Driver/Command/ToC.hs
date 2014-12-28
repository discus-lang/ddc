
module DDC.Driver.Command.ToC
        ( cmdToSeaFromFile
        , cmdToSeaSourceTetraFromFile
        , cmdToSeaSourceTetraFromString
        , cmdToSeaCoreFromFile
        , cmdToSeaCoreFromString)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Base.Pretty
import System.FilePath
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Build.Interface.Store        (Store)


-------------------------------------------------------------------------------
-- | Convert a module to C.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToSeaFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Core language definition.
        -> ExceptT String IO ()

cmdToSeaFromFile config store filePath
 
 -- Convert a Disciple Source Tetra module.
 | ".ds"          <- takeExtension filePath
 =      cmdToSeaSourceTetraFromFile config store filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language  <- languageOfExtension (takeExtension filePath)
 =      cmdToSeaCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to C."


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to C.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSeaSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToSeaSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSeaSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to C.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSeaSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToSeaSourceTetraFromString config store source str
 = let  
        pipeLoad
         = pipeText (nameOfSource source) (lineStartOfSource source) str
         $ stageSourceTetraLoad config source store
         [ PipeCoreReannotate (const ())
         [ stageTetraToSalt   config source 
         [ stageSaltOpt       config source
         [ stageSaltToC       config source SinkStdout]]]]
         
   in do
        errs    <- liftIO pipeLoad
        case errs of
         []     -> return ()
         es     -> throwE $ renderIndent $ vcat $ map ppr es


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to Sea.
--   Works for the 'Tetra', 'Lite' and 'Salt' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToSeaCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToSeaCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSeaCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Parse, check, and convert a module to C.
--   The output is printed to @stdout@. 
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToSeaCoreFromString  
        :: Config       -- ^ Compiler configuration.
        -> Language     -- ^ Language definition.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ExceptT String IO ()

cmdToSeaCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = do   
        let fragName = profileName profile
        
        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Convert a Core Tetra module to C.
                | fragName == "Tetra"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageTetraLoad     config source
                [ stageTetraToSalt   config source
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]]
                
                -- Convert a Core Lite module to C.
                | fragName == "Lite"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageLiteLoad      config source
                [ stageLiteOpt       config source 
                [ stageLiteToSalt    config source 
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]]]

                -- Convert a Core Lite module to Salt.
                | fragName == "Salt"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ stageSaltLoad      config source
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]

                -- Unrecognised.
                | otherwise
                = throwE $ "Cannot convert '" ++ fragName ++ "'modules to C."


        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwE $ renderIndent $ vcat $ map ppr es

