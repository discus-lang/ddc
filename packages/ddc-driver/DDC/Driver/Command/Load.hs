
module DDC.Driver.Command.Load
        ( cmdLoad
        , cmdReadModule)
where
import DDC.Driver.Source
import DDC.Driver.Bundle
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Module
import DDC.Core.Load
import DDC.Core.Pretty
import DDC.Data.Canned
import Control.Monad
import Data.IORef
import System.Directory


-- | Load and transform a module.
cmdLoad :: Bundle -> Source -> String -> IO ()
cmdLoad bundle source str
 | Bundle fragment _ zero simpl _    <- bundle
 = do   errs    <- pipeText (nameOfSource source) (lineStartOfSource source) str
                $  PipeTextLoadCore  fragment
                [  PipeCoreSimplify  fragment zero simpl
                [  PipeCoreCheck     fragment
                [  PipeCoreOutput    SinkStdout ]]]

        mapM_ (putStrLn . renderIndent . ppr) errs


cmdReadModule 
        :: (Ord n, Show n, Pretty n)
        => Fragment n err 
        -> FilePath 
        -> IO (Maybe (Module (AnTEC () n) n))

cmdReadModule frag filePath
 = do
        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath
        let source   = SourceFile filePath

        cmdReadModule_parse frag source src


cmdReadModule_parse frag source src
 = do   ref     <- newIORef Nothing
        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore frag
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref (Just m) >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                putStrLn "ok"
                readIORef ref

         _ -> do
                mapM_ (putStrLn . renderIndent . ppr) errs
                return Nothing
