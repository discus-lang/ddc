
-- | Interactive interface to DDC for core programs.
--   This also accepts interpreter commands as arguments on the command-line.
--   If you want a more unixy inferface then use the ddc-main interface instead.
module Main where
import DDCI.Core.Command.Help
import DDCI.Core.Interface.Args
import DDCI.Core.Interface.Batch
import DDCI.Core.Interface.Interactive
import DDCI.Core.State
import DDC.Driver.Command.Compile
import DDC.Driver.Interface.Input
import System.Environment
import System.IO
import Control.Monad.Trans.Except
import Data.List
import qualified DDC.Core.Interface.Store       as Store

main :: IO ()
main
 = do   args    <- getArgs
        case args of
         []      -> runInteractive

         -- Display the help.
         ["--help"]
          ->    putStr help

         -- Run commands from a file in batch mode.
         ["--batch", filePath]
          -> do file    <- readFile filePath
                runBatch filePath file

         -- Make a file, depending on what the extension is.
         -- This gets us  --make on the command line as well as -make
         -- so we behave more like GHC.
         ["--make",  filePath]
          -> do let state       = initState (InputInterfaceBatch filePath)
                dconfig         <- getDriverConfigOfState state
                store           <- Store.new
                runError $ cmdCompileRecursive dconfig True store [filePath]

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dcx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file


         _       -> runArgs args


-- | Just print errors to stdout and continue the session.
runError :: ExceptT String IO a -> IO ()
runError m
 = do   result  <- runExceptT m
        case result of
         Left err       -> hPutStrLn stdout err
         Right _        -> return ()

