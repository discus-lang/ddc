
module DDC.Driver.Command.ToPHP
        ( cmdToPHPFromFile
        , cmdToPHPSourceTetraFromFile
        , cmdToPHPSourceTetraFromString
        , cmdToPHPCoreFromFile
        , cmdToPHPCoreFromString)
where
import DDC.Driver.Stage
import DDC.Driver.Command.Compile
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
import DDC.Build.Interface.Store        (Store)
import DDC.Core.Exp.Annot.AnTEC
import qualified DDC.Build.Interface.Store      as Store
import qualified DDC.Driver.Stage.Tetra         as DE
import qualified DDC.Core.Transform.Reannotate  as CReannotate


-------------------------------------------------------------------------------
-- | Convert a module to PHP.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToPHPFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Core language definition.
        -> ExceptT String IO ()

cmdToPHPFromFile config store filePath
 
 -- Convert a Disciple Source Tetra module.
 | ".ds"          <- takeExtension filePath
 =      cmdToPHPSourceTetraFromFile config store filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language  <- languageOfExtension (takeExtension filePath)
 =      cmdToPHPCoreFromFile config language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to PHP."


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to PHP.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToPHPSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToPHPSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Call the compiler to build/load all dependent modules.
        cmdCompileRecursive config False store filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToPHPSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to PHP.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToPHPSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store                -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToPHPSourceTetraFromString config store source str
 = withExceptT (renderIndent . vcat . map ppr)
 $ do  
        modSalt' 
         <- do  modTetra <- DE.sourceLoadText config store  source str
                return modTetra

        errs
         <- liftIO $ pipeCore modSalt'
         $   PipeCoreReannotate annotTail
              [ PipeCoreAsTetra
              [ PipeTetraToPHP     SinkStdout ]]

        case errs of
         []     -> return ()
         _      -> throwE errs


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to PHP.
--   Works for the 'Tetra', and 'Salt' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToPHPCoreFromFile
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToPHPCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToPHPCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Parse, check, and convert a module to PHP.
--   The output is printed to @stdout@. 
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToPHPCoreFromString  
        :: Config       -- ^ Compiler configuration.
        -> Language     -- ^ Language definition.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ExceptT String IO ()

cmdToPHPCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = withExceptT (renderIndent . vcat . map ppr)
 $ do   
        let fragName = profileName profile
        store   <- liftIO Store.new

        -- Decide what to do based on file extension and current fragment.
        let makeSalt
                |  fragName == "Tetra"
                =  fmap (CReannotate.reannotate (const ()))
                $  DE.tetraLoadText config store source str

                -- Unrecognised.
                | otherwise
                = throwE [ErrorLoad $ "Cannot convert '" ++ fragName ++ "'modules to C."]

        modTetra <- makeSalt

        errs    <-  liftIO $ pipeCore modTetra
                $   PipeCoreAsTetra
                [   PipeTetraToPHP     SinkStdout]

        -- Throw any errors that arose during compilation
        case errs of
         []     -> return ()
         _      -> throwE errs

