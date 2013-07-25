
module DDCI.Tetra.Input
        ( InputState    (..)
        , Input         (..) 
        , readInput
        , eatLine)
where
import DDCI.Tetra.State
import DDCI.Tetra.Command
import DDC.Interface.Input


-- Eating input lines.
eatLine :: State 
        -> InputState Command 
        -> String 
        -> IO (State, InputState Command)

eatLine state inputState chunk
 = do  (inputState', mCmdLine)    
         <- inputLine (stateInterface state) inputState chunk
   
       case mCmdLine of
         Nothing        
          ->    return  (state, inputState')

         Just (source, Nothing,  line)
          -> do state'  <- handleCommand state CommandHelp source line
                return  (state', inputState')

         Just (source, Just cmd, line)
          -> do state'  <- handleCommand state cmd source line
                return  (state', inputState')         

