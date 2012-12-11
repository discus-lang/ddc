
module DDC.Driver.Command.Load
        ( cmdReadModule
        , cmdLoadFromFile
        , cmdLoadFromString)
where
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Simplifier.Parser
import DDC.Core.Module
import DDC.Core.Load
import DDC.Core.Pretty
import DDC.Data.Canned
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.IORef
import System.Directory
import System.FilePath
import System.IO
import qualified DDC.Core.Transform.Inline.Templates    as I
import qualified Data.Map                               as Map


-- Read -----------------------------------------------------------------------
-- | Load and typecheck a module.
cmdReadModule 
        :: (Ord n, Show n, Pretty n, NFData n)
        => Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> IO (Maybe (Module (AnTEC () n) n))

cmdReadModule frag filePath
 = do
        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath
        let source   = SourceFile filePath

        cmdReadModule_parse filePath frag source src


cmdReadModule_parse filePath frag source src
 = do   ref     <- newIORef Nothing
        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore frag
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref (Just m) >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                readIORef ref

         _ -> do
                putStrLn $ "When reading " ++ filePath
                mapM_ (hPutStrLn stderr . renderIndent . ppr) errs
                return Nothing


-------------------------------------------------------------------------------
-- | Load and transform a module, printing the result to stdout.
--   The current transform is set with the given string.
cmdLoadFromFile
        :: Maybe String         -- ^ Simplifier specification.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdLoadFromFile strSimpl filePath
 = case languageOfExtension (takeExtension filePath) of
        Nothing       -> throwError $ "Unknown file extension."
        Just language -> cmdLoad_language strSimpl filePath language

cmdLoad_language Nothing filePath language
 = configLoad_simpl language filePath

cmdLoad_language (Just strSimpl) filePath language
 | Language bundle      <- language
 , modules              <- bundleModules       bundle
 , rules                <- bundleRewriteRules  bundle
 , mkNamT               <- bundleMakeNamifierT bundle
 , mkNamX               <- bundleMakeNamifierX bundle
 = let
        rules'          = Map.assocs rules

        -- Collect all definitions from modules
        localTemplates  = I.lookupTemplateFromModules
                        $ Map.elems modules

        -- Module specific templates.
        importTemplates = map (\(n,m) -> (n, I.lookupTemplateFromModules [m]))
                        $ Map.assocs modules

        -- Simplifier details for the parser.
        details         = SimplifierDetails mkNamT mkNamX rules' 
                                localTemplates
                                importTemplates

   in   case parseSimplifier details strSimpl of
         Nothing
          -> throwError $ "Transform spec parse error."

         Just simpl
          -> let bundle' = bundle { bundleSimplifier = simpl }
             in  configLoad_simpl (Language bundle') filePath

configLoad_simpl language filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdLoadFromString language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Load and transform a module, 
--   then print the result to @stdout@.
cmdLoadFromString
        :: Language             -- ^ Language definition
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdLoadFromString language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment   bundle
 , simpl                <- bundleSimplifier bundle
 , zero                 <- bundleStateInit  bundle
 = do   errs    <- liftIO
                $  pipeText (nameOfSource source) (lineStartOfSource source) str
                $  PipeTextLoadCore  fragment
                [  PipeCoreSimplify  fragment zero simpl
                [  PipeCoreCheck     fragment
                [  PipeCoreOutput    SinkStdout ]]]

        case errs of
         [] -> return ()
         es -> throwError $ renderIndent $ vcat $ map ppr es

