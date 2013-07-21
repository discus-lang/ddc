
module DDC.Driver.Command.Load
        ( cmdReadModule
        , cmdReadModule'
        , cmdLoadFromFile
        , cmdLoadFromString)
where
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Simplifier.Parser
import DDC.Core.Transform.Reannotate
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
import qualified Data.Map               as Map
import qualified DDC.Base.Parser        as BP


-- Read -----------------------------------------------------------------------
-- | Load and typecheck a module.
cmdReadModule 
        :: (Ord n, Show n, Pretty n, NFData n)
        => Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> IO (Maybe (Module (AnTEC BP.SourcePos n) n))
cmdReadModule = cmdReadModule' True


cmdReadModule'
        :: (Ord n, Show n, Pretty n, NFData n)
        => Bool                 -- ^ If true, print errors out
        -> Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> IO (Maybe (Module (AnTEC BP.SourcePos n) n))

cmdReadModule' printErrors frag filePath
 = do
        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath
        let source   = SourceFile filePath

        cmdReadModule_parse printErrors filePath frag source src


cmdReadModule_parse printErrors filePath frag source src
 = do   ref     <- newIORef Nothing
        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore frag
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref (Just m) >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                readIORef ref

         _ -> do
                when printErrors
                 $ do putStrLn $ "When reading " ++ filePath
                      mapM_ (hPutStrLn stderr . renderIndent . ppr) errs
                return Nothing


-------------------------------------------------------------------------------
-- | Load and transform a module, printing the result to stdout.
--   The current transform is set with the given string.
cmdLoadFromFile
        :: Maybe String         -- ^ Simplifier specification.
        -> [FilePath]           -- ^ More modules to use as inliner templates.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdLoadFromFile strSimpl fsTemplates filePath
 = case languageOfExtension (takeExtension filePath) of
        Nothing       -> throwError $ "Unknown file extension."
        Just language -> cmdLoad_language strSimpl fsTemplates filePath language

cmdLoad_language Nothing _ filePath language
 = configLoad_simpl language filePath

cmdLoad_language (Just strSimpl) fsTemplates filePath language
 | Language bundle      <- language
 , modules              <- bundleModules       bundle
 , rules                <- bundleRewriteRules  bundle
 , mkNamT               <- bundleMakeNamifierT bundle
 , mkNamX               <- bundleMakeNamifierX bundle
 , fragment             <- bundleFragment      bundle
 , readName             <- fragmentReadName    fragment
 = do
        let rules'          = Map.assocs rules

        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        mMoreModules
         <- liftM sequence
         $  mapM (liftIO . cmdReadModule fragment)
                 fsTemplates

        let zapAnnot annot
                = annot { annotTail = () }

        moreModules
         <- case mMoreModules of
                 Nothing -> throwError $ "Imported modules do not parse."
                 Just ms -> return     $ map (reannotate zapAnnot) ms

        -- Collect all definitions from modules
        let templateModules
                = moreModules ++ (Map.elems modules)

        -- Simplifier details for the parser.
        let details
                = SimplifierDetails mkNamT mkNamX 
                        [(n, reannotate zapAnnot rule) | (n, rule) <- rules']
                        templateModules

        case parseSimplifier readName details strSimpl of
         Left err
          -> throwError $ renderIndent $ ppr err

         Right simpl
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
                [  PipeCoreReannotate (\a -> a { annotTail = ()})
                [  PipeCoreSimplify  fragment zero simpl
                [  PipeCoreCheck     fragment
                [  PipeCoreOutput    SinkStdout ]]]]

        case errs of
         [] -> return ()
         es -> throwError $ renderIndent $ vcat $ map ppr es

