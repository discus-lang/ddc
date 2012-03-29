
import DDCI.Core.State
import DDCI.Core.Build.Make
import DDCI.Core.Command
import DDCI.Core.Interface.Interactive
import DDCI.Core.Interface.Input
import System.Environment
import Data.List


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []      -> runInteractive

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dcx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file

         ["--batch", filePath]
          -> do file    <- readFile filePath
                runBatch filePath file

         -- Make a file, depending on what the extension is.
         ["--make",  filePath]
          -> do let state       = initState (InputBatch filePath)
                makeFile state filePath
        
         _       -> runArgs args



-- Batch ----------------------------------------------------------------------
-- | Run in batch mode, reading commands from the given string.
runBatch :: FilePath -> String -> IO ()
runBatch filePath str
 = do   let state       = initState (InputBatch filePath)
        let inputState  = InputState Nothing InputLine 1 []
        loop state inputState (lines str)
 where 
        -- No more lines, we're done.
        -- There might be a command in the buffer though.
        loop state inputState []
         = do   eatLine state inputState []
                return ()

        loop state inputState (l:ls)
         -- Echo comment lines back.
         |  isPrefixOf "--" l
         = do   putStrLn l
                let inputState'
                        = inputState 
                        { inputLineNumber = inputLineNumber inputState + 1 }

                loop state inputState' ls

         -- Echo blank lines back if we're between commands.
         | Nothing      <- inputCommand inputState
         , null l
         = do   putStr "\n"
                let inputState'
                        = inputState 
                        { inputLineNumber = inputLineNumber inputState + 1 }

                loop state inputState' ls

         -- Quit the program.
         | isPrefixOf ":quit" l
         = do   return ()

         -- Handle a line of input.
         | otherwise
         = do   (state', inputState')  <- eatLine state inputState l
                loop state' inputState' ls


-- Args -----------------------------------------------------------------------
-- | Run in unix command-line mode, reading commands from a list of arguments.
runArgs :: [String] -> IO ()
runArgs args
 = do   let state    = initState InputArgs
        loop state args
 where 
        -- No more args, we're done.
        loop _state []
         = do   return ()

        loop state (('-':cmdColon) : file : rest)
         | isSuffixOf ":" cmdColon
         , cmdStr               <- init cmdColon
         , Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   contents        <- readFile file
                state'          <- handleCmd state cmd 0 contents
                loop state' rest

        loop state (('-':cmdStr) : arg : rest)
         | Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   state'          <- handleCmd state cmd 0 arg
                loop state' rest

        loop _state xs
         = error $ "bad args " ++ (show xs)

