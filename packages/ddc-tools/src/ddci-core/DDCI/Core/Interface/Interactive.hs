
module DDCI.Core.Interface.Interactive
        ( runInteractive)
where
import DDCI.Core.Input
import DDCI.Core.State
import DDCI.Core.Command
import DDC.Interface.Input
import Data.List
import Data.Maybe
import qualified System.Console.Haskeline       as HL
import qualified System.Console.Haskeline.IO    as HL


-- | Run an interactive session, reading commands from the console.
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.3.3: http://disciple.ouroborus.net."
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
         = do   -- If this isn't the first line then print the prompt.
                let prompt = if isJust (inputCommand inputState)
                             then ""
			     else if isJust (stateTransInteract state)
			     then "trans> "
                             else "> "
         
                -- Read a line from the user and echo it back.
                line    <- getInput hlState prompt

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

