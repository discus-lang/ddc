
module DDC.Driver.Command.Read
        ( cmdReadModule
        , cmdReadModule')
where
import DDC.Interface.Source
import DDC.Driver.Config
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Module
import DDC.Core.Load
import DDC.Core.Pretty
import DDC.Data.Canned
import Control.DeepSeq
import Control.Monad
import Data.IORef
import System.Directory
import System.IO
import qualified DDC.Base.Parser                as BP
import qualified DDC.Core.Check                 as C


-- Read -----------------------------------------------------------------------
-- | Load and typecheck a module.
cmdReadModule 
        :: (Ord n, Show n, Pretty n, Pretty (err (AnTEC BP.SourcePos n)), NFData n)
        => Config               -- ^ Driver config.
        -> Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> IO (Maybe (Module (AnTEC BP.SourcePos n) n))
cmdReadModule = cmdReadModule' True


cmdReadModule'
        :: (Ord n, Show n, Pretty n, Pretty (err (AnTEC BP.SourcePos n)), NFData n)
        => Bool                 -- ^ If true, print errors out
        -> Config               -- ^ Driver config
        -> Fragment n err       -- ^ Language fragment.
        -> FilePath             -- ^ Path to the module.
        -> IO (Maybe (Module (AnTEC BP.SourcePos n) n))

cmdReadModule' printErrors config frag filePath
 = do
        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath
        let source   = SourceFile filePath

        cmdReadModule_parse printErrors config filePath frag source src


cmdReadModule_parse printErrors config filePath frag source src
 = do   ref     <- newIORef Nothing
        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore frag 
                        (if configInferTypes config then C.Synth else C.Recon)
                        SinkDiscard
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref (Just m) >> return m)) 
                     [PipeCoreOutput pprDefaultMode SinkDiscard] ]

        case errs of
         [] -> do
                readIORef ref

         _ -> do
                when printErrors
                 $ do putStrLn $ "When reading " ++ filePath
                      mapM_ (hPutStrLn stderr . renderIndent . ppr) errs
                return Nothing

