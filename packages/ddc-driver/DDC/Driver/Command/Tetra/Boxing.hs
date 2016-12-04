
module DDC.Driver.Command.Tetra.Boxing
        (cmdTetraBoxing)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Core.Check                 as C
import qualified DDC.Data.Pretty                as P
import qualified DDC.Build.Language.Tetra       as Tetra


-- | Manage boxing in a Tetra module.
cmdTetraBoxing
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdTetraBoxing config source sourceText

 -- Curring only works for Disciple Core Tetra files.
 | SourceFile filePath  <- source
 , ext  <- takeExtension filePath 
 , ext /= ".dct"
 = throwE $ "The Boxing transform only works for Core Tetra (.dct) modules."

 | otherwise
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipeBoxing
         = pipeText     (nameOfSource source) 
                        (lineStartOfSource source) sourceText
         $ PipeTextLoadCore Tetra.fragment C.Recon SinkDiscard
         [ PipeCoreAsTetra
         [ PipeTetraBoxing
         [ PipeCoreCheck  "TetraBoxing" Tetra.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]]
   in do
        errs    <- liftIO pipeBoxing
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es


