
module DDC.Driver.Command.Flow.ToTetra
        ( cmdFlowToTetraFromFile
        , cmdFlowToTetraCoreFromFile
        , cmdFlowToTetraCoreFromString
        , pipelineFlowToTetra)
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
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import qualified DDC.Build.Language.Salt        as Salt 
import qualified DDC.Core.Salt                  as Salt 
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow                  as Flow
import qualified DDC.Core.Check                 as C

import DDC.Data.Canned
import qualified DDC.Core.Flow.Transform.Concretize     as Concretize

-------------------------------------------------------------------------------
-- | Convert a module to Core Tetra.
--   The output is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdFlowToTetraFromFile
        :: Config               -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdFlowToTetraFromFile config configLower filePath

 -- Convert a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 =      cmdFlowToTetraCoreFromFile config configLower language filePath

 -- Don't know how to convert this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot convert '" ++ ext ++ "' files to Tetra."


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Tetra.
--   Works for the 'Flow' fragment.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdFlowToTetraCoreFromFile
        :: Config               -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()  

cmdFlowToTetraCoreFromFile config configLower language filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        cmdFlowToTetraCoreFromString config configLower language (SourceFile filePath) src


-------------------------------------------------------------------------------
-- | Convert some fragment of Disciple Core to Core Tetra.
--   Works for the 'Lite' and 'Tetra' fragments.
--   The result is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
cmdFlowToTetraCoreFromString
        :: Config               -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Language             -- ^ Language definition.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdFlowToTetraCoreFromString config configLower language source str
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
                -- Compile a Core Tetra module to Tetra.
                | fragName == "Flow"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) str
                $ pipelineFlowToTetra config configLower source
                [ PipeCoreCheck    Salt.fragment C.Recon SinkDiscard
                  []
                , PipeCoreOutput   pmode SinkStdout]

                -- Unrecognised fragment name or file extension.
                | otherwise
                = throwE $ "Cannot convert '" ++ fragName ++ "' modules to Salt."

        -- Throw any errors that arose during compilation
        errs <- compile
        case errs of
         []     -> return ()
         es     -> throwE $ renderIndent $ vcat $ map ppr es


pipelineFlowToTetra 
        :: Config 
        -> Flow.Config 
        -> Source 
        -> [PipeCore () Salt.Name] 
        -> PipeText Flow.Name Flow.Error

pipelineFlowToTetra config configLower source pipesSalt
 = stageFlowLoad    config source
 [ stageFlowRate    config source
 [ stageFlowPrep  config source
 [ PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
 [ stageFlowLower   config configLower source
 [ PipeCoreHacks (Canned $ \m -> return $ Concretize.concretizeModule m)
 [ PipeCoreCheck    Flow.fragment C.Recon SinkDiscard
 [ stageFlowWind    config source
 [ PipeCoreCheck    Flow.fragment C.Recon SinkDiscard
 [ stageFlowToTetra config source
   pipesSalt]]]]]]]]]
