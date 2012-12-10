
module DDC.Main.Command.Load
        (cmdLoad)
where
import DDC.Core.Pretty
import DDC.Main.Config
import DDC.Driver.Bundle
import DDC.Build.Language
import DDC.Build.Pipeline
import DDC.Core.Simplifier.Parser
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import System.Directory
import qualified DDC.Core.Transform.Inline.Templates    as I
import qualified Data.Map                               as Map


-- | Load and transform a module, printing the result to stdout.
--   The current transform is set with the given string.
cmdLoad :: Config       -- ^ DDC config.
        -> Maybe String -- ^ Simplifier specification.
        -> FilePath     -- ^ Module file name.
        -> ErrorT String IO ()

cmdLoad config strSimpl filePath
 = case bundleFromFilePath config filePath of
        Nothing      -> throwError $ "Unknown file extension."
        Just bundle  -> cmdLoad_bundle config strSimpl filePath bundle 

cmdLoad_bundle config Nothing filePath bundle
 = configLoad_simpl config bundle filePath

cmdLoad_bundle config (Just strSimpl) filePath bundle
 | Bundle frag modules _ _ rules       <- bundle
 , Fragment _ _ _ _ _ _ mkNamT mkNamX zero      <- frag
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
          -> let bundle' = Bundle frag modules zero simpl rules 
             in  configLoad_simpl config bundle' filePath

configLoad_simpl _config bundle filePath
 | Bundle frag _ zero simpl _          <- bundle
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath
        errs    <- liftIO
                $  pipeText filePath 0 src
                $  PipeTextLoadCore frag
                [  PipeCoreSimplify     frag zero simpl
                [  PipeCoreCheck        frag
                [  PipeCoreOutput       SinkStdout ]]]

        case errs of
         [] -> return ()
         es -> throwError $ renderIndent $ vcat $ map ppr es
   
