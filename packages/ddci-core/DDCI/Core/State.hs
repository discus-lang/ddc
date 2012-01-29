
module DDCI.Core.State
        ( State(..)
        , TransformSpec(..)
        , initState

        , Mode (..)
        , adjustMode)
where
import DDCI.Core.Mode
import DDCI.Core.TransformSpec
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Interpreter state.
data State
        = State
        { stateModes            :: Set Mode 
        , stateTransformSpec    :: TransformSpec }


-- | Adjust a mode setting in the state.
adjustMode 
        :: Bool         -- ^ Whether to enable or disable the mode.        
        -> Mode         -- ^ Mode to adjust.
        -> State
        -> State

adjustMode True mode state
        = state { stateModes    = Set.insert mode (stateModes state) }

adjustMode False mode state
        = state { stateModes    = Set.delete mode (stateModes state) }


-- | The initial state.
initState :: State
initState
        = State
        { stateModes            = Set.empty 
        , stateTransformSpec    = None }

