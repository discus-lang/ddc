
import DDCI.Core.Command.Help
import DDCI.Core.Interface.Args
import DDCI.Core.Interface.Batch
import DDCI.Core.Interface.Interactive
import DDCI.Core.State
import DDCI.Core.Build.Make
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
         ["--make",  filePath]
          -> do let state       = initState (InterfaceBatch filePath)
                makeFile state filePath

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dcx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file

        
         _       -> runArgs args


