
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
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Control.Monad
import qualified DDC.Build.Language.Tetra       as Tetra
import qualified DDC.Build.Language.Lite        as Lite
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Core.Check                 as C


-------------------------------------------------------------------------------
-- | Convert a module to C.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSeaFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Core language definition.
        -> ErrorT String IO ()

cmdToSeaFromFile config filePath
 
 -- Convert a Disciple Source Tetra module.
 | ".dst"         <- takeExtension filePath
 =      cmdToSeaSourceTetraFromFile config filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language  <- languageOfExtension (takeExtension filePath)
 =      cmdToSeaCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwError $ "Cannot convert '" ++ ext ++ "' files to C."


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to C.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdToSeaSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdToSeaSourceTetraFromFile config filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSeaSourceTetraFromString config (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to C.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdToSeaSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdToSeaSourceTetraFromString config source str
 = let  
        pipeLoad
         = pipeText (nameOfSource source) (lineStartOfSource source) str
         $ stageSourceTetraLoad config source
         [ PipeCoreReannotate (const ())
         [ stageTetraToSalt   config source 
         [ stageSaltOpt       config source
         [ stageSaltToC       config source SinkStdout]]]]
         
   in do
        errs    <- liftIO pipeLoad
        case errs of
         []     -> return ()
         es     -> throwError $ renderIndent $ vcat $ map ppr es


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to Sea.
--   Works for the 'Tetra', 'Lite' and 'Salt' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSeaCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdToSeaCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSeaCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Parse, check, and convert a module to C.
--   The output is printed to @stdout@. 
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSeaCoreFromString  
        :: Config       -- ^ Compiler configuration.
        -> Language     -- ^ Language definition.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

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
                $ PipeTextLoadCore Tetra.fragment C.Recon SinkDiscard
                [ PipeCoreReannotate (const ())
                [ stageTetraToSalt   config source
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]]]
                
                -- Convert a Core Lite module to C.
                | fragName == "Lite"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore Lite.fragment C.Recon SinkDiscard
                [ PipeCoreReannotate (const ())
                [ stageLiteOpt       config source 
                [ stageLiteToSalt    config source 
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]]]]

                -- Convert a Core Lite module to Salt.
                | fragName == "Salt"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore   Salt.fragment C.Recon SinkDiscard
                [ PipeCoreReannotate (const ())
                [ stageSaltOpt       config source
                [ stageSaltToC       config source SinkStdout]]]

                -- Unrecognised.
                | otherwise
                = throwError $ "Cannot convert '" ++ fragName ++ "'modules to C."


        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwError $ renderIndent $ vcat $ map ppr es

