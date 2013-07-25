
module DDCI.Core.Input
        ( InputState    (..)
        , Input         (..) 
        , readInput
        , eatLine)
where
import DDCI.Core.State
import DDCI.Core.Command
import DDCI.Core.Command.TransInteract
import DDC.Interface.Input


-- Eating input lines.
eatLine :: State -> InputState Command 
        -> String 
        -> IO (State, InputState Command)

eatLine state inputState@(InputState mCommand mode lineNumber acc) chunk
 | Just _ <- stateTransInteract state
 = do   state' <- cmdTransInteractLoop state chunk
	return (state', InputState mCommand mode (lineNumber+1) acc)

 | otherwise 
 = do  (inputState', mCmdLine)    
                <- inputLine (stateInterface state) readCommand inputState chunk
   
       case mCmdLine of
         Nothing        
          ->    return  (state, inputState')

         Just (source, Nothing,  line)
          -> do state'  <- handleCmd state CommandEval source line
                return  (state', inputState')

         Just (source, Just cmd, line)
          -> do state'  <- handleCmd state cmd source line
                return  (state', inputState')         

