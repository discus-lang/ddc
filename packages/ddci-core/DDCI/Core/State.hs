
module DDCI.Core.State
        ( State         (..)
        , Interface     (..)
        , Source        (..)

        , Language      (..)
        , languages
	, stateRewriteRulesList

        , Transform     (..)
        , initState

        , Mode          (..)
        , adjustMode)
where
import DDCI.Core.Pipeline.Transform
import DDCI.Core.Language
import DDCI.Core.Mode
import DDC.Core.Transform.Rewrite.Rule
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
        , stateLanguage         :: Language
        , stateTransform        :: Transform
	, stateRewriteRules	:: Map String (RewriteRule () Name) }


-- | What interface is being used.
data Interface
        -- | Read commands from unix command-line args.
        = InterfaceArgs

        -- | Read commands interactively from the console.
        | InterfaceConsole

        -- | Read commands from the file with this name.
        | InterfaceBatch        FilePath
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
        , stateLanguage         = Language fragmentEval
        , stateTransform        = Transform1 TransformId
	, stateRewriteRules	= Map.empty  }

stateRewriteRulesList :: State -> [RewriteRule () Name]
stateRewriteRulesList State { stateRewriteRules = rules }
 = map snd $ Map.toList rules

