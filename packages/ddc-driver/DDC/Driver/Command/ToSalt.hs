
module DDC.Driver.Command.ToSalt
        ( cmdToSaltFromFile
        , cmdToSaltSourceTetraFromFile
        , cmdToSaltSourceTetraFromString
        , cmdToSaltCoreFromFile
        , cmdToSaltCoreFromString)
where
import DDC.Driver.Stage
import DDC.Driver.Config
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
-- | Convert a module to Core Salt.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSaltFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdToSaltFromFile config filePath

 -- Convert a Disciple Source Tetra module.
 | ".dst"        <- takeExtension filePath
 =      cmdToSaltSourceTetraFromFile config filePath
 
 -- Convert a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 =      cmdToSaltCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwError $ "Cannot convert '" ++ ext ++ "' files to Salt."


-------------------------------------------------------------------------------
-- | Convert Disciple Core Tetra to Disciple Core Salt.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdToSaltSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdToSaltSourceTetraFromFile config filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSaltSourceTetraFromString config (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Core Tetra to Disciple Core Salt.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdToSaltSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdToSaltSourceTetraFromString config source str
 = let  
        pmode   = prettyModeOfConfig $ configPretty config

        pipeLoad
         = pipeText (nameOfSource source)
                    (lineStartOfSource source) str
         $ stageSourceTetraLoad config source
         [ PipeCoreReannotate (const ())
         [ stageTetraToSalt     config source 
         [ stageSaltOpt         config source
         [ PipeCoreCheck        Salt.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]]]

   in do
        errs    <- liftIO pipeLoad
        case errs of
         []     -> return ()
         es     -> throwError $ renderIndent $ vcat $ map ppr es


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Salt.
--   Works for the 'Lite' and 'Tetra' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSaltCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()  

cmdToSaltCoreFromFile config language filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSaltCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Salt.
--   Works for the 'Lite' and 'Tetra' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
cmdToSaltCoreFromString
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdToSaltCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , profile              <- fragmentProfile fragment
 = do   
        -- Language fragment name.
        let fragName    = profileName profile

        -- Pretty printer mode.
        let pmode       = prettyModeOfConfig $ configPretty config

        -- Decide what to do based on the fragment name.
        let compile
                -- Compile a Core Tetra module to Salt.
                | fragName == "Tetra"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore Tetra.fragment C.Recon SinkDiscard
                [ PipeCoreReannotate (const ())
                [ stageTetraToSalt config source
                [ stageSaltOpt     config source
                [ PipeCoreCheck    Salt.fragment C.Recon SinkDiscard
                [ PipeCoreOutput   pmode SinkStdout]]]]]
                
                -- Convert a Core Lite module to Salt.
                | fragName == "Lite" 
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore Lite.fragment C.Recon SinkDiscard
                [ PipeCoreReannotate (const ())
                [ stageLiteOpt     config source
                [ stageLiteToSalt  config source
                [ stageSaltOpt     config source
                [ PipeCoreCheck    Salt.fragment C.Recon SinkDiscard
                [ PipeCoreOutput   pmode SinkStdout]]]]]]

                -- Unrecognised fragment name or file extension.
                | otherwise
                = throwError 
                $ "Cannot convert '" ++ fragName ++ "' modules to Salt."

        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwError $ renderIndent $ vcat $ map ppr es

