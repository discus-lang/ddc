
module DDCI.Core.State
        ( State         (..)
        , initState

        , Interface     (..)
        , Source        (..)

        , Language      (..)
        , languages
	, stateRewriteRulesList

        , Mode          (..)
        , adjustMode)
where
import DDCI.Core.Language
import DDCI.Core.Mode
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Eval.Name               (Name)
import Data.Map                         (Map)
import Data.Set                         (Set)
import DDC.Core.Simplifier              (Simplifier)
import qualified DDC.Core.Simplifier    as S
import qualified Data.Map               as Map
import qualified Data.Set               as Set


-- | Interpreter state.
--   This is adjusted by interpreter commands.
data State
        = State
        { -- | ddci interface state.
          stateInterface        :: Interface

          -- | ddci mode flags.
        , stateModes            :: Set Mode 

          -- | Source language to accept.
        , stateLanguage         :: Language

          -- | Simplifier to apply to core program.
        , stateSimplifier       :: Simplifier

          -- | Rewrite rules to apply during simplification.
	, stateRewriteRules	:: Map String (RewriteRule () Name) 

          -- | Output file for @compile@ and @make@ commands.
        , stateOutputFile       :: Maybe FilePath 

          -- | Output dir for @compile@ and @make@ commands
        , stateOutputDir        :: Maybe FilePath }


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
        , stateSimplifier       = S.Trans S.Id
	, stateRewriteRules	= Map.empty  
        , stateOutputFile       = Nothing
        , stateOutputDir        = Nothing }


stateRewriteRulesList :: State -> [RewriteRule () Name]
stateRewriteRulesList State { stateRewriteRules = rules }
 = map snd $ Map.toList rules

