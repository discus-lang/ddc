
module DDCI.Core.State
        ( State         (..)
        , Interface     (..)

        , StateProfile  (..)
        , stateProfiles
	, stateRewriteRulesList

        , Transform(..)
        , initState

        , Mode (..)
        , adjustMode)
where
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Fragment
import DDCI.Core.Pipeline.Transform
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
        { stateInterface        :: Interface
        , stateModes            :: Set Mode 
        , stateTransform        :: Transform
	, stateRewriteRules	:: Map String (RewriteRule () Name) 
        , stateProfile          :: StateProfile }


-- | What interface is being used.
data Interface
        -- | Read commands from unix command-line args.
        = InterfaceArgs

        -- | Read commands from the file with this name.
        | InterfaceBatch    FilePath

        -- | Read commands interactively from the console.
        | InterfaceInteractive
        deriving (Eq, Show)


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
initState :: Interface -> State
initState interface
        = State
        { stateInterface        = interface
        , stateModes            = Set.empty 
        , stateTransform        = None
	, stateRewriteRules	= Map.empty 
        , stateProfile          = StateProfile evalProfile }

stateRewriteRulesList :: State -> [RewriteRule () Name]
stateRewriteRulesList State { stateRewriteRules = rules }
 = map snd $ Map.toList rules

