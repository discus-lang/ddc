
module DDC.Driver.Command.ToShimmer
        ( cmdToShimmerFromFile
        , cmdToShimmerSourceTetraFromFile
        , cmdToShimmerSourceTetraFromString
        , cmdToShimmerCoreFromFile
        , cmdToShimmerCoreFromString)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Stage
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
import qualified DDC.Driver.Stage.Tetra         as DE


-------------------------------------------------------------------------------
-- | Convert a module to Shimmer.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToShimmerFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToShimmerFromFile config store filePath

 -- Convert a Disciple Source Tetra module.
 | ".ds"         <- takeExtension filePath
 =      cmdToShimmerSourceTetraFromFile config store filePath
 
 -- Convert a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 =      cmdToShimmerCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to SMR."


-------------------------------------------------------------------------------
-- | Convert Disciple Core Tetra to Shimmer.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToShimmerSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToShimmerSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Call the compiler to build/load all dependent modules.
        cmdCompileRecursive config False store filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToShimmerSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to Shimmer
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToShimmerSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToShimmerSourceTetraFromString config store source str
 = withExceptT (renderIndent . vcat . map ppr)
 $ do  
        modSMR
         <-  DE.tetraToShimmer config source 
         =<< DE.sourceLoadText config store  source str

        liftIO $ putStr $ renderIndent $ ppr modSMR


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Shimmer.
--   Works for the 'Tetra' fragment.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToShimmerCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()  

cmdToShimmerCoreFromFile config language filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToShimmerCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Shimmer
--   Works for the 'Tetra' fragment.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToShimmerCoreFromString
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()               

cmdToShimmerCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , profile              <- fragmentProfile fragment
 = withExceptT (renderIndent . vcat . map ppr)
 $ do   
        -- Language fragment name.
        let fragName    = profileName profile
        store           <- liftIO Store.new 

        -- Make shimmer from the input file.
        let makeSMR
                |   fragName == "Tetra"
                =   DE.tetraToShimmer config source
                =<< DE.tetraLoadText  config store source str

                -- Unrecognised.
                | otherwise
                = throwE [ErrorLoad $ "Cannot convert '" ++ fragName ++ "'modules to SMR."]

        modSMR  <- makeSMR

        liftIO $ putStr $ renderIndent $ ppr modSMR

