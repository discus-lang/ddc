
module DDC.Driver.Command.Load
        ( cmdLoadFromFile
        , cmdLoadFromString)
where
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Simplifier.Parser
import DDC.Core.Transform.Reannotate
import DDC.Driver.Command.Read
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


-- TODO: split out simplifier parser into different module.
-- Don't mash simplifier parsing in with this command.


-- | Load and transform a module, printing the result to stdout.
--   The current transform is set with the given string.
cmdLoadFromFile
        :: Config
        -> Maybe String         -- ^ Simplifier specification.
        -> [FilePath]           -- ^ More modules to use as inliner templates.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdLoadFromFile config strSimpl fsTemplates filePath
 = let ext      = takeExtension filePath
   in case languageOfExtension ext of
        Nothing         -> throwError $ "Cannot load '" ++ ext ++ "' files."
        Just language   -> cmdLoad_language config strSimpl fsTemplates filePath language

cmdLoad_language config Nothing _ filePath language
 = configLoad_simpl config language filePath

cmdLoad_language config (Just strSimpl) fsTemplates filePath language
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
             in  configLoad_simpl config (Language bundle') filePath

configLoad_simpl config language filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdLoadFromString config language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Load and type check a module, printing 
--   the result to @stdout@.
cmdLoadFromString
        :: Config
        -> Language             -- ^ Language definition
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdLoadFromString config language source str
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

