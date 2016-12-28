
module DDC.Driver.Command.ToLlvm
        ( cmdToLlvmFromFile
        , cmdToLlvmSourceTetraFromFile
        , cmdToLlvmSourceTetraFromString
        , cmdToLlvmCoreFromFile
        , cmdToLlvmCoreFromString)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Stage
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Data.Pretty
import System.Directory
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Build.Interface.Store                (Store)
import qualified DDC.Build.Interface.Store      as Store
import qualified DDC.Driver.Stage.Tetra         as DE
import qualified DDC.Driver.Stage.Salt          as DA
import qualified DDC.Core.Transform.Reannotate  as CReannotate


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

        -- Call the compiler to build/load all dependent modules.
        cmdCompileRecursive config False store filePath

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
 = withExceptT (renderIndent . vcat . map ppr)
 $ do  
        modSalt' 
         <- do  modTetra <- DE.sourceLoadText config store  source str
                modSalt  <- DE.tetraToSalt    config source modTetra
                return modSalt

        errs
         <- liftIO $ pipeCore modSalt'
         $  stageSaltOpt           config source
          [ stageSaltToSlottedLLVM config source
          [ PipeLlvmPrint SinkStdout]]
 
        case errs of
         []     -> return ()
         _      -> throwE errs


-------------------------------------------------------------------------------
-- | Parse, check and convert a Core module to LLVM.
--   Works for the 'Tetra', and 'Salt' fragments.
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
--   Works for the 'Tetra', and 'Salt' fragments.
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
 = withExceptT (renderIndent . vcat . map ppr)
 $ do   
        let fragName    =  profileName profile
        store           <- liftIO $ Store.new

        -- Decide what to do based on file extension and current fragment.
        let makeSalt
                |   fragName == "Tetra"
                =   DE.tetraToSalt   config source
                =<< DE.tetraLoadText config store source str

                |   fragName == "Salt"
                =   fmap (CReannotate.reannotate (const ()))
                $   DA.saltLoadText  config store source str

                -- Unrecognised.
                | otherwise
                = throwE [ErrorLoad $ "Cannot convert '" ++ fragName ++ "'modules to C."]

        modSalt <- makeSalt

        errs    <- liftIO $ pipeCore modSalt
                $  stageSaltOpt config source 
                 [ (if fragName == "Tetra" 
                        then stageSaltToSlottedLLVM   config source
                        else stageSaltToUnSlottedLLVM config source)
                 [ PipeLlvmPrint      SinkStdout]]

        -- Throw any errors that arose during compilation
        case errs of
         []     -> return ()
         _      -> throwE errs

