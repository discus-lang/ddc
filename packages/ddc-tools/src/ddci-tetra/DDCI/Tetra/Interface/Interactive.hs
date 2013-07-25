
module DDCI.Tetra.Interface.Interactive
        (runInteractive)
where
import DDCI.Tetra.Input
import DDCI.Tetra.State
import DDCI.Tetra.Command
import DDC.Interface.Input
import Data.List
import Data.Maybe
import qualified System.Console.Haskeline       as HL
import qualified System.Console.Haskeline.IO    as HL


-- | Run an interactive session, reading commands from the console.
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-tetra, version 0.3.2: http://disciple.ouroborus.net."
        putStrLn "Type :help for help."

        -- Setup terminal mode.
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = do   hlState         <- HL.initializeInput HL.defaultSettings
        let state       = initState InputInterfaceConsole
        
        let inputState  
                = InputState 
                { inputParseCommand     = readCommand
                , inputMode             = InputLine
                , inputCommand          = Nothing
                , inputLineNumber       = 1
                , inputAcc              = [] }
                
        loop state inputState hlState
 where  
        loop state inputState hlState 
         = do   -- Read a line from the user and echo it back.
                line    <- getInput hlState " >"

                if  isPrefixOf ":quit" line
                 || isPrefixOf ":q"    line
                 then return ()
                 else do
                        (state', inputState')
                                <- eatLine state inputState line

                        loop state' inputState' hlState


-- | Get an input line from the console, using given prompt
getInput :: HL.InputState -> String -> IO String
getInput hlState prompt
 = do   line <- HL.queryInput hlState (HL.getInputLine prompt)
        return (fromMaybe ":quit" line)
