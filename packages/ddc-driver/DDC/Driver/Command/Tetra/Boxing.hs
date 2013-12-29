
module DDC.Driver.Command.Tetra.Boxing
        (cmdTetraBoxing)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Check                 as C
import qualified DDC.Base.Pretty                as P
import qualified DDC.Build.Language.Tetra       as Tetra


-- | Prepare a Disciple Core Tetra module for lowering.
cmdTetraBoxing
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdTetraBoxing config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipeBoxing
         = pipeText     (nameOfSource source) 
                        (lineStartOfSource source) sourceText
         $ PipeTextLoadCore Tetra.fragment C.Recon SinkDiscard
         [ PipeCoreAsTetra
         [ PipeTetraBoxing
         [ PipeCoreCheck  Tetra.fragment C.Recon
         [ PipeCoreOutput pmode SinkStdout ]]]]
   in do
        errs    <- liftIO pipeBoxing
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es


