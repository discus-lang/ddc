
module DDCI.Core.State
        ( State         (..)
        , Bundle        (..)
        , initState

        , Interface     (..)
        , Source        (..)

        , Language      (..)
        , languages
        , getActiveBuilder

        , Mode          (..)
        , adjustMode)
where
import DDCI.Core.Mode
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Core.Check
import DDC.Core.Module
import DDC.Core.Simplifier
import DDC.Core.Transform.Rewrite.Rule
import DDC.Base.Pretty
import Data.Typeable
import Data.Map                         (Map)
import Data.Set                         (Set)
import qualified DDC.Core.Salt.Name     as Salt
import qualified DDC.Core.Lite.Name     as Lite
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
        , stateBundle           :: Bundle

          -- | Maps of modules we can use as inliner templates.
        , stateWithLite         :: Map ModuleName (Module (AnTEC () Lite.Name) Lite.Name)
        , stateWithSalt         :: Map ModuleName (Module (AnTEC () Salt.Name) Salt.Name)

          -- | Simplifier to apply to core program.
        , stateSimplLite        :: Simplifier Int (AnTEC () Lite.Name) Lite.Name
        , stateSimplSalt        :: Simplifier Int (AnTEC () Salt.Name) Salt.Name

          -- | Force the builder to this one, this sets the address width etc.
          --   If Nothing then query the host system for the default builder.
        , stateBuilder          :: Maybe Builder

          -- | Output file for @compile@ and @make@ commands.
        , stateOutputFile       :: Maybe FilePath 

          -- | Output dir for @compile@ and @make@ commands
        , stateOutputDir        :: Maybe FilePath }


-- | Existential container for a language fragment, 
--      the simplifier for it,
--      and the dictionaries we need to work with its type parameters.
data Bundle
        = forall s n err
        .  (Typeable n, Ord n, Show n, Pretty n, Pretty (err (AnTEC () n)))
        => Bundle 
        {  bundleFragment        :: Fragment n err
	,  bundleModules	 :: Map ModuleName (Module (AnTEC () n) n)
        ,  bundleStateInit       :: s
        ,  bundleSimplifier      :: Simplifier s (AnTEC () n) n
        ,  bundleRewriteRules    :: Map String (RewriteRule (AnTEC () n) n) }


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
        , stateBundle           = Bundle fragmentEval Map.empty () (S.Trans S.Id) Map.empty
        , stateWithLite         = Map.empty
        , stateWithSalt         = Map.empty
        , stateSimplLite        = S.Trans S.Id
        , stateSimplSalt        = S.Trans S.Id
        , stateBuilder          = Nothing  
        , stateOutputFile       = Nothing
        , stateOutputDir        = Nothing }


-- | Get the active builder.
--   If one is set explicitly in the state then use that, 
--   otherwise query the host system to determine the builder.
--   If that fails as well then 'error'.
getActiveBuilder :: State -> IO Builder 
getActiveBuilder state 
 = case stateBuilder state of
        Just builder          -> return builder
        Nothing         
         -> do  mBuilder <- determineDefaultBuilder defaultBuilderConfig
                case mBuilder of
                 Nothing      -> error "getActiveBuilder unrecognised host platform"
                 Just builder -> return builder


