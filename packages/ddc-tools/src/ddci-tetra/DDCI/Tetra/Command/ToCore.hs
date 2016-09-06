module DDCI.Tetra.Command.ToCore
        (cmdToCore)
where
import DDCI.Tetra.State
import DDC.Driver.Interface.Source
import DDC.Driver.Stage
import DDC.Build.Pipeline   
import DDC.Data.Pretty
import qualified DDC.Driver.Config      as D
import DDC.Build.Interface.Store        (Store)

-- Convert Disciple Source Tetra text into Disciple Core Tetra.
cmdToCore :: State -> D.Config -> Store -> Source -> String -> IO ()
cmdToCore _state config store source str
 = let  
        pmode   = D.prettyModeOfConfig $ D.configPretty config

        pipeLoad
         = pipeText (nameOfSource source) (lineStartOfSource source) str
         $ stageSourceTetraLoad config source store
         [ PipeCoreOutput pmode SinkStdout ]

   in do
        errs    <- pipeLoad
        case errs of
         []     -> return ()
         es     -> putStrLn (renderIndent $ ppr es)

