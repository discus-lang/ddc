
module DDCI.Tetra.State
        ( State (..)
        , initState 
        , adjustMode)
where
import DDCI.Tetra.Mode
import DDC.Interface.Input
import Data.Set                 (Set)
import qualified Data.Set       as Set

data State
        = State
        { -- ddci interface state
          stateInterface        :: InputInterface 

          -- ddci mode flags.
        , stateModes            :: Set Mode }


initState :: InputInterface -> State
initState interface
        = State
        { stateInterface        = interface 

        , stateModes            = Set.empty }


-- | Adjust a mode setting in the state.
adjustMode 
        :: Bool         -- ^ Whether to enable or disable the mode.        
        -> Mode         -- ^ Mode to adjust.
        -> State -> State

adjustMode True mode state
        = state { stateModes    = Set.insert mode (stateModes state) }

adjustMode False mode state
        = state { stateModes    = Set.delete mode (stateModes state) }
