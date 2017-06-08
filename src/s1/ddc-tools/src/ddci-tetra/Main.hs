
-- | Interactive interface to DDC for Tetra programs.
--   This also accepts interpreter commands as arguments on the command-line.
--   If you want a more unixy inferface then use the ddc-main interface instead.
module Main where
import DDCI.Tetra.Command.Help
import DDCI.Tetra.Interface.Args
import DDCI.Tetra.Interface.Batch
import DDCI.Tetra.Interface.Interactive
import System.Environment
import Data.List


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> runInteractive

         -- Display the help.
         ["--help"]
          ->    putStr help

         -- Run commands from a file in batch mode.
         ["--batch", filePath]
          -> do file    <- readFile filePath
                runBatch filePath file

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dsx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file

        
         _      -> runArgs args

