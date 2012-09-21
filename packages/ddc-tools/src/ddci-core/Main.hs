
-- | Interactive interface to DDC.
--      This also accepts interpreter commands as arguments on the command-line.
--      If you want a more unixy inferface then use the ddc-main interface instead.
module Main where
import DDCI.Core.Command.Make
import DDCI.Core.Command.Help
import DDCI.Core.Interface.Args
import DDCI.Core.Interface.Batch
import DDCI.Core.Interface.Interactive
import DDCI.Core.State
import System.Environment
import Data.List


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
          -> do let state       = initState (InterfaceBatch filePath)
                cmdMake state (SourceFile filePath) filePath

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dcx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file

        
         _       -> runArgs args

