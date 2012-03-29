
module DDCI.Core.Interface.Interactive
        ( runInteractive)
where
import DDCI.Core.Interface.Input
import DDCI.Core.State
import Data.List
import Data.Maybe
import qualified System.Console.Haskeline       as HL
import qualified System.Console.Haskeline.IO    as HL


-- Interactive ----------------------------------------------------------------
-- | Run an interactive session, reading commands from the console.
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.3.0: http://disciple.ouroborus.net.   :? for help"

        -- Setup terminal mode.
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = do   hlState         <- HL.initializeInput HL.defaultSettings
        let state       = initState InputInteractive
        let inputState  = InputState Nothing InputLine 1 []
        loop state inputState hlState
 where  
        loop state inputState hlState 
         = do   -- If this isn't the first line then print the prompt.
                let prompt = if isNothing (inputCommand inputState)
                                then "> "
                                else ""
         
                -- Read a line from the user and echo it back.
                line    <- getInput hlState prompt

                if isPrefixOf ":quit" line
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

