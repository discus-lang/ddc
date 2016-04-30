
module DDCI.Tetra.State
        ( State (..)
        , initState 
        , adjustMode

        -- * Driver Config
        , getDriverConfigOfState
        , getDefaultBuilderConfig)
where
import DDCI.Tetra.Mode
import DDC.Code.Config
import DDC.Driver.Interface.Input
import DDC.Build.Builder
import System.FilePath
import Data.Set                         (Set)
import qualified DDC.Core.Simplifier    as S
import qualified DDC.Driver.Stage       as D
import qualified DDC.Driver.Config      as D
import qualified DDC.Core.Salt.Runtime  as Runtime
import qualified Data.Set               as Set


-------------------------------------------------------------------------------
data State
        = State
        { -- ddci interface state
          stateInterface        :: InputInterface 

          -- ddci mode flags.
        , stateModes            :: Set Mode 

          -- Force the builder to this one, which sets the address width etc.
          -- If `Nothing` then query the host system for the default builder.
        , stateBuilder          :: Maybe Builder }


-- | Initial ddci-tetra state.
initState :: InputInterface -> State
initState interface
        = State
        { stateInterface        = interface 
        , stateModes            = Set.empty 
        , stateBuilder          = Nothing }


-- | Adjust a mode setting in the state.
adjustMode 
        :: Bool         -- ^ Whether to enable or disable the mode.        
        -> Mode         -- ^ Mode to adjust.
        -> State -> State

adjustMode True mode state
        = state { stateModes    = Set.insert mode (stateModes state) }

adjustMode False mode state
        = state { stateModes    = Set.delete mode (stateModes state) }


-- Driver Config --------------------------------------------------------------
-- | Slurp out the relevant parts of the ddci-tetra state into a driver config.
--   The driver config controls how the compiler treats core files.
--
getDriverConfigOfState :: State -> IO D.Config
getDriverConfigOfState state
 = do   builder <- getActiveBuilder state
        return 
         $ D.Config
         { D.configLogBuild                     = True
         , D.configDump                         = Set.member Dump  (stateModes state)
         , D.configInferTypes                   = False
         , D.configViaBackend                   = D.ViaLLVM

         -- ISSUE #300: Allow the default heap size to be set when
         --   compiling the program.
         , D.configRuntime
                = Runtime.Config
                { Runtime.configHeapSize = 65536 }

         , D.configRuntimeLinkStrategy          = D.LinkDefault
         , D.configModuleBaseDirectories        = []
         , D.configOutputFile                   = Nothing
         , D.configOutputDir                    = Nothing
         , D.configSimplSalt                    = S.Trans S.Id
         , D.configBuilder                      = builder
         , D.configPretty                       = configPretty
         , D.configSuppressHashImports          = False
         , D.configKeepLlvmFiles                = False
         , D.configKeepSeaFiles                 = False
         , D.configKeepAsmFiles                 = False 

         , D.configTaintAvoidTypeChecks         = False
        }

 where  modes   = stateModes state
        configPretty   
         = D.ConfigPretty
         { D.configPrettyVarTypes               = Set.member PrettyVarTypes   modes
         , D.configPrettyConTypes               = Set.member PrettyConTypes   modes
         , D.configPrettyUseLetCase             = Set.member PrettyUseLetCase modes 
         , D.configPrettySuppressImports        = Set.member SuppressImports  modes
         , D.configPrettySuppressExports        = Set.member SuppressExports  modes
         , D.configPrettySuppressLetTypes       = Set.member SuppressLetTypes modes }


-- | Holds platform independent builder info.
getDefaultBuilderConfig :: IO BuilderConfig
getDefaultBuilderConfig
 = do   baseLibraryPath <- locateBaseLibrary
        return $ BuilderConfig
          { builderConfigBaseSrcDir             = baseLibraryPath
          , builderConfigBaseLibDir             = baseLibraryPath </> "build"
          , builderConfigLibFile                = \_static dynamic -> dynamic }


-- | Get the active builder.
--   If one is set explicitly in the state then use that, 
--   otherwise query the host system to determine the builder.
--   If that fails as well then 'error'.
getActiveBuilder :: State -> IO Builder 
getActiveBuilder state 
 = case stateBuilder state of
        Just builder          -> return builder
        Nothing         
         -> do  config   <- getDefaultBuilderConfig 
                mBuilder <- determineDefaultBuilder config
                case mBuilder of
                 Nothing      -> error "ddci-tetra.getActiveBuilder: unrecognised host platform"
                 Just builder -> return builder

