
module DDCI.Tetra.State
        ( State (..)
        , initState )
where
import DDC.Interface.Input

data State
        = State
        { -- ddci interface state
          stateInterface        :: InputInterface 

          -- dump intermediate Core Tetra file.
        , stateDumpCore         :: Bool }


initState :: InputInterface -> State
initState interface
        = State
        { stateInterface        = interface 

        , stateDumpCore         = False }
