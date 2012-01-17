
module DDCI.Core.State
        ( State(..)
        , initState

        , Mode (..)
        , applyMode)
where
import DDCI.Core.Mode
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Interpreter state.
data State
        = State
        { stateModes    :: Set Mode }


-- | Apply a mode setting to the state.
applyMode :: Bool -> Mode -> State -> State
applyMode True mode state
        = state { stateModes    = Set.insert mode (stateModes state) }

applyMode False mode state
        = state { stateModes    = Set.delete mode (stateModes state) }


initState :: State
initState
        = State
        { stateModes    = Set.empty }

