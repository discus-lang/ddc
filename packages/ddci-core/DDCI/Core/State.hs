
module DDCI.Core.State
        ( State         (..)
        , StateProfile  (..)
	, stateRewriteRulesList
        , Transform(..)
        , initState

        , Mode (..)
        , adjustMode)
where
import DDCI.Core.Fragment
import DDCI.Core.Mode
import DDCI.Core.Transform
import DDC.Core.Language.Profile
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Name               (Name)
import Data.Map                         (Map)
import Data.Set                         (Set)
import qualified Data.Map               as Map
import qualified Data.Set               as Set


-- | Interpreter state.
data State
        = State
        { stateModes            :: Set Mode 
        , stateTransform        :: Transform
	, stateRewriteRules	:: Map String (RewriteRule () Name) 
        , stateProfile          :: StateProfile }

data StateProfile
        = forall n err. Fragment n err
        => StateProfile (Profile n)


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
        , stateTransform        = None
	, stateRewriteRules	= Map.empty 
        , stateProfile          = StateProfile evalProfile }

stateRewriteRulesList :: State -> [RewriteRule () Name]
stateRewriteRulesList State { stateRewriteRules = rules }
 = map snd $ Map.toList rules

