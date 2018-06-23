
module DDC.Driver.Command.ToSalt
        ( cmdToSaltFromFile
        , cmdToSaltSourceTetraFromFile
        , cmdToSaltSourceTetraFromString
        , cmdToSaltCoreFromFile
        , cmdToSaltCoreFromString)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Data.Pretty
import System.FilePath
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Build.Interface.Store                (Store)
import qualified DDC.Build.Interface.Store      as Store
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Core.Check                 as C
import qualified DDC.Driver.Stage.Tetra         as DE
import qualified DDC.Driver.Stage.Salt          as DA


-------------------------------------------------------------------------------
-- | Convert a module to Core Salt.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSaltFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToSaltFromFile config store filePath

 -- Convert a Disciple Source Tetra module.
 | ".ds"         <- takeExtension filePath
 =      cmdToSaltSourceTetraFromFile config store filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 =      cmdToSaltCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to Salt."


-------------------------------------------------------------------------------
-- | Convert Disciple Core Tetra to Disciple Core Salt.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSaltSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToSaltSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Call the compiler to build/load all dependent modules.
        cmdCompileRecursive config False store [filePath]

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSaltSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to Disciple Core Salt.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSaltSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToSaltSourceTetraFromString config store source str
 = withExceptT (renderIndent . vcat . map ppr)
 $ do
        let pmode   = prettyModeOfConfig $ configPretty config

        modSalt'
         <-  DA.saltSimplify   config source
         =<< DE.discusToSalt   config source []
         =<< DE.sourceLoadText config store  source str

        errs
         <- liftIO $ pipeCore modSalt'
         $   PipeCoreCheck      "ToSaltSourceTetraFromString"
                                Salt.fragment C.Recon SinkDiscard
           [ PipeCoreOutput pmode SinkStdout ]

        case errs of
         []     -> return ()
         _      -> throwE errs


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Salt.
--   Works for the 'Tetra' fragment.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSaltCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToSaltCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToSaltCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Salt.
--   Works for the 'Tetra' fragment.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToSaltCoreFromString
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToSaltCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , profile              <- fragmentProfile fragment
 = withExceptT (renderIndent . vcat . map ppr)
 $ do
        -- Language fragment name.
        let fragName    = profileName profile
        store           <- liftIO Store.new

        -- Pretty printer mode.
        let pmode       = prettyModeOfConfig $ configPretty config

        -- Make salt from the input file.
        let makeSalt
                |   fragName == "Discus"
                =   DA.saltSimplify   config source
                =<< DE.discusToSalt   config source []
                =<< DE.discusLoadText config store source str

                |   fragName == "Salt"
                =   DA.saltSimplify  config source
                =<< DA.saltLoadText  config store source str

                -- Unrecognised.
                | otherwise
                = throwE [ErrorLoad $ "Cannot convert '" ++ fragName ++ "'modules to Salt."]

        modSalt <- makeSalt

        errs    <- liftIO $ pipeCore modSalt
                $  PipeCoreOutput pmode SinkStdout

        -- Throw any errors that arose during compilation
        case errs of
         []     -> return ()
         _      -> throwE errs


