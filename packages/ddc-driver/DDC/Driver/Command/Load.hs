
module DDC.Driver.Command.Load
        ( cmdLoadFromFile
        , cmdLoadSourceTetraFromFile
        , cmdLoadSourceTetraFromString
        , cmdLoadCoreFromFile
        , cmdLoadCoreFromString
        , cmdLoadSimplifier)
where
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Simplifier.Parser
import DDC.Core.Transform.Reannotate
import DDC.Driver.Command.Read
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Core.Annot.AnTEC
import DDC.Core.Pretty
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import qualified Data.Map                       as Map
import qualified DDC.Core.Check                 as C


-------------------------------------------------------------------------------
-- | Load and transform a module, printing the result to stdout.
--
--   Handle fragments of Disciple Core, where the current fragment is determined
--   by expecting the file name extension.
--      
--   This function also takes a textual description of the simplifier to apply
--   to the module.
--
cmdLoadFromFile
        :: Config
        -> Maybe String         -- ^ Simplifier specification.
        -> [FilePath]           -- ^ More modules to use as inliner templates.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdLoadFromFile config mStrSimpl fsTemplates filePath

 -- Load a Disciple Source Tetra module.
 | ".dst"        <- takeExtension filePath
 =      cmdLoadSourceTetraFromFile config filePath

 -- Load a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 = case mStrSimpl of
        Nothing 
         ->     cmdLoadCoreFromFile config language filePath
        
        Just strSimpl
         -> do  language' <- cmdLoadSimplifier language strSimpl fsTemplates
                cmdLoadCoreFromFile config language' filePath

 -- Don't know how to load this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwError $ "Cannot load '" ++ ext ++ "' files."


-------------------------------------------------------------------------------
-- | Load a Disciple Source Tetra module from a file.
--   The result is printed to @stdout@.
cmdLoadSourceTetraFromFile
        :: Config
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdLoadSourceTetraFromFile config filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdLoadSourceTetraFromString config (SourceFile filePath) src


-------------------------------------------------------------------------------
cmdLoadSourceTetraFromString
        :: Config
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdLoadSourceTetraFromString config source str
 = let
        pmode   = prettyModeOfConfig $ configPretty config

        pipeLoad
         = pipeText     (nameOfSource source) (lineStartOfSource source) str
         $ stageSourceTetraLoad config source
         [ PipeCoreOutput pmode SinkStdout ]
   in do
        errs    <- liftIO pipeLoad
        case errs of
         [] -> return ()
         es -> throwError $ renderIndent $ vcat $ map ppr es
 

-------------------------------------------------------------------------------
-- | Load some fragment of Disciple core from a file.
--   The result is printed to @stdout@.
cmdLoadCoreFromFile
        :: Config
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path
        -> ErrorT String IO ()

cmdLoadCoreFromFile config language filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdLoadCoreFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Load a Disciple Core module from a string.
--   The result it printed to @stdout@.
cmdLoadCoreFromString
        :: Config
        -> Language             -- ^ Language definition
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdLoadCoreFromString config language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment   bundle
 = let  
        pmode           = prettyModeOfConfig $ configPretty config

        pipeLoad
         = pipeText     (nameOfSource source) (lineStartOfSource source) str
         $ PipeTextLoadCore  fragment 
                        (if configInferTypes config then C.Synth else C.Recon) 
                        SinkDiscard
         [ PipeCoreOutput    pmode SinkStdout ]

   in do
        errs    <- liftIO pipeLoad
        case errs of
         [] -> return ()
         es -> throwError $ renderIndent $ vcat $ map ppr es


-------------------------------------------------------------------------------
-- | Parse the simplifier defined in this string, 
--   and load it and all the inliner templates into the language bundle.
cmdLoadSimplifier 
        :: Language
        -> String               -- ^ Simplifier specification.
        -> [FilePath]           -- ^ Modules to use as inliner templates.
        -> ErrorT String IO Language

cmdLoadSimplifier language strSimpl fsTemplates
 | Language bundle      <- language
 , modules_bundle       <- bundleModules bundle
 , mkNamT               <- bundleMakeNamifierT bundle
 , mkNamX               <- bundleMakeNamifierX bundle
 , rules                <- bundleRewriteRules bundle
 , fragment             <- bundleFragment      bundle
 , readName             <- fragmentReadName    fragment
 = do
        -- Load all the modues that we're using for inliner templates.
        --  If any of these don't load then the 'cmdReadModule' function 
        --  will display the errors.
        mModules
         <- liftM sequence
         $  mapM (liftIO . cmdReadModule fragment)
                 fsTemplates

        modules_annot
         <- case mModules of
                 Nothing -> throwError $ "Cannot load inlined module."
                 Just ms -> return     $ ms

        -- Zap annotations on the loaded modules.
        -- Any type errors will already have been displayed, so we don't need 
        -- the source position info any more.
        let zapAnnot annot
                = annot { annotTail = () }

        let modules_new 
                = map (reannotate zapAnnot) modules_annot

        -- Collect all definitions from modules
        let modules_all
                = Map.elems modules_bundle ++ modules_new

        -- Wrap up the inliner templates and current rules into
        -- a SimplifierDetails, which we give to the Simplifier parser.
        let details
                = SimplifierDetails mkNamT mkNamX 
                    [(n, reannotate zapAnnot rule) | (n, rule) <- Map.assocs rules]
                    modules_all

        -- Parse the simplifer string.
        case parseSimplifier readName details strSimpl of
         Left err
          -> throwError $ renderIndent $ ppr err

         Right simpl
          -> return $ Language $ bundle { bundleSimplifier = simpl }

