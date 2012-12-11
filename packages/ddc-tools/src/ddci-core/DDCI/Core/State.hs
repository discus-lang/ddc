
module DDCI.Core.State
        ( State         (..)
        , Bundle        (..)
        , initState
        , getDriverConfigOfState

        , TransHistory	(..)

        , Interface     (..)
        , Source        (..)

        , Language      (..)
        , languages
        , defaultBuilderConfig
        , getActiveBuilder

        , Mode          (..)
        , adjustMode)
where
import DDCI.Core.Mode
import DDC.Driver.Source
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Simplifier
import DDC.Core.Transform.Rewrite.Rule
import DDC.Base.Pretty
import Data.Typeable
import DDC.Core.Transform.Namify                (Namifier)
import DDC.Type.Env                             (Env)
import DDC.Core.Fragment                        (Profile)
import Data.Map                                 (Map)
import Data.Set                                 (Set)
import qualified DDC.Build.Language.Eval        as Eval
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Core.Lite                  as Lite
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Salt.Runtime          as Runtime
import qualified DDC.Driver.Stage               as D
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


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

          -- | Maps of modules we can use as inliner templates.
        , stateWithLite         :: Map ModuleName (Module (AnTEC () Lite.Name) Lite.Name)
        , stateWithSalt         :: Map ModuleName (Module (AnTEC () Salt.Name) Salt.Name)

          -- | Simplifier to apply to core program.
        , stateSimplLite        :: Simplifier Int () Lite.Name
        , stateSimplSalt        :: Simplifier Int () Salt.Name

          -- | Force the builder to this one, this sets the address width etc.
          --   If Nothing then query the host system for the default builder.
        , stateBuilder          :: Maybe Builder

          -- | Output file for @compile@ and @make@ commands.
        , stateOutputFile       :: Maybe FilePath 

          -- | Output dir for @compile@ and @make@ commands
        , stateOutputDir        :: Maybe FilePath

	  -- | Interactive transform mode
	, stateTransInteract	:: Maybe TransHistory}



data TransHistory
	= forall s n
        .  (Typeable n, Ord n, Show n, Pretty n)
	=> TransHistory
	{ -- | Original expression and its types
	  historyExp		:: (Exp (AnTEC () n) n, Type n, Effect n, Closure n) 
	  -- | Keep history of steps so we can go back and construct final sequence
	, historySteps		:: [(Exp (AnTEC () n) n, Simplifier s (AnTEC () n) n)]

	  -- | We need to keep these around so we know they're same type
	  --   I must be doing this wrong.
	  --   But I tried using casts and would have needed to derive
	  --   Data.Typeable on Env, RewriteRule, everything.
	, historyMakeNamifierT	:: Env n -> Namifier s n
	, historyMakeNamifierX	:: Env n -> Namifier s n 
	, historyNameZero	:: s
	, historyProfile	:: Profile n 
	, historyModules	:: Map ModuleName (Module (AnTEC () n) n) 
        , historyRewriteRules   :: Map String (RewriteRule (AnTEC () n) n) }



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
        , stateLanguage         = Eval.language
        , stateWithLite         = Map.empty
        , stateWithSalt         = Map.empty
        , stateSimplLite        = S.Trans S.Id
        , stateSimplSalt        = S.Trans S.Id
        , stateBuilder          = Nothing  
        , stateOutputFile       = Nothing
        , stateOutputDir        = Nothing
	, stateTransInteract	= Nothing }


-- | Slurp out the relevant parts of the DDCI stage into a driver config.
getDriverConfigOfState :: State -> IO D.Config
getDriverConfigOfState state
 = do   builder <- getActiveBuilder state
        return 
         $ D.Config
         { D.configDump                  = Set.member Dump (stateModes state)
         , D.configViaBackend            = D.ViaLLVM

         -- TODO: be able to set this from command-line option
         , D.configRuntime
                = Runtime.Config
                { Runtime.configHeapSize = 65536 }

         , D.configOutputFile            = stateOutputFile state
         , D.configOutputDir             = stateOutputDir  state
         , D.configSimplLite             = stateSimplLite  state
         , D.configSimplSalt             = stateSimplSalt  state
         , D.configBuilder               = builder
         , D.configSuppressCoreImports   = Set.member SuppressImports (stateModes state)
         , D.configSuppressHashImports   = not $ Set.member SaltPrelude (stateModes state) 
         , D.configKeepLlvmFiles         = False
         , D.configKeepSeaFiles          = False
         , D.configKeepAsmFiles          = False }


-- | Holds platform independent builder info.
defaultBuilderConfig :: BuilderConfig
defaultBuilderConfig
        = BuilderConfig
        { builderConfigRuntime  = "code/salt" }


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


