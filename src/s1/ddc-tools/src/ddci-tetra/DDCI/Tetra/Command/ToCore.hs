module DDCI.Tetra.Command.ToCore
        (cmdToCore)
where
import DDCI.Tetra.State
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline   
import DDC.Data.Pretty
import DDC.Build.Interface.Store        (Store)
import qualified DDC.Driver.Config      as D
import qualified DDC.Driver.Stage.Tetra         as DE
import Control.Monad.Trans.Except
import Control.Monad.IO.Class


-- Convert Disciple Source Tetra text into Disciple Core Tetra.
cmdToCore :: State -> D.Config -> Store -> Source -> String -> IO ()
cmdToCore _state config store source str
 = do
        result
         <- runExceptT
         $  do  let pmode = D.prettyModeOfConfig $ D.configPretty config

                modTetra <- DE.sourceLoadText config store source str

                errs     <- liftIO $ pipeCore modTetra
                         $  PipeCoreOutput pmode SinkStdout

                case errs of
                 []     -> return ()
                 _      -> throwE errs

        case result of
         Left errs -> putStrLn (renderIndent $ ppr errs)
         Right _   -> return ()

