
module DDC.Driver.Command.ToLlvm
        ( cmdToLlvmFromFile
        , cmdToLlvmSourceTetraFromFile
        , cmdToLlvmSourceTetraFromString
        , cmdToLlvmCoreFromFile
        , cmdToLlvmCoreFromString)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Interface.Source
import DDC.Driver.Stage
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Data.Pretty
import System.Directory
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Core.Interface.Store                 (Store)
import qualified DDC.Core.Interface.Store       as Store
import qualified DDC.Driver.Stage.Tetra         as DE
import qualified DDC.Driver.Stage.Salt          as DA
import qualified DDC.Core.Discus                as D
import qualified DDC.Llvm.Write                 as Llvm
import qualified System.IO                      as S

-------------------------------------------------------------------------------
-- | Convert a module to LLVM.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdToLlvmFromFile
        :: Config               -- ^ Driver config.
        -> Store D.Name         -- ^ Interface store.
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
        -> Store D.Name         -- ^ Interface store.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdToLlvmSourceTetraFromFile config store filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Call the compiler to build/load all dependent modules.
        cmdCompileRecursive config False store [filePath]

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdToLlvmSourceTetraFromString config store (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert Disciple Source Tetra to LLVM.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdToLlvmSourceTetraFromString
        :: Config               -- ^ Driver config.
        -> Store D.Name         -- ^ Interface store.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdToLlvmSourceTetraFromString config store source str
 = withExceptT (renderIndent . vcat . map ppr)
 $ do
        modLlvm'
         <-  DA.saltToLlvm     config source True
         =<< DA.saltSimplify   config source
         =<< DE.discusToSalt   config source
         =<< DE.sourceLoadText config store  source str

        liftIO $ Llvm.write (Llvm.configOfHandle S.stdout) modLlvm'


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
                =   DA.saltSimplify   config source
                =<< DE.discusToSalt   config source
                =<< DE.discusLoadText config store source str

                |   fragName == "Salt"
                =   DA.saltSimplify  config source
                =<< DA.saltLoadText  config source str

                -- Unrecognised.
                | otherwise
                = throwE [ErrorLoad $ "Cannot convert '" ++ fragName ++ "'modules to C."]

        modSalt <- makeSalt

        -- Convert Core Salt to LLVM.
        let bSlotify = fragName == "Tetra"
        modLlvm <- DA.saltToLlvm config source bSlotify modSalt

        -- Print LLVM code to stdout.
        liftIO $ Llvm.write (Llvm.configOfHandle S.stdout) modLlvm


