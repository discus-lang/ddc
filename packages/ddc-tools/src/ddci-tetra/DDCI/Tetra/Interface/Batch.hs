
module DDCI.Tetra.Interface.Batch
        (runBatch)
where
import DDCI.Tetra.State
import DDCI.Tetra.Input
import DDCI.Tetra.Command
import DDC.Interface.Input
import Data.List


-- | Run in batch mode, reading commands from the given string.
runBatch :: FilePath -> String -> IO ()
runBatch filePath str
 = do   let state       = initState (InputInterfaceBatch filePath)
        
        let inputState  
                = InputState 
                { inputParseCommand     = readCommand
                , inputMode             = InputLine
                , inputCommand          = Nothing
                , inputLineNumber       = 1
                , inputAcc              = [] }


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

