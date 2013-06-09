{-# LANGUAGE CPP #-}

-- | Define the command line configuation arguments.
module DDC.Main.Config
        ( Mode         (..)
        , OptLevel     (..)
        , D.ViaBackend (..)
        , Config       (..)

        , getDefaultConfig
        , defaultBuilderConfig)
where
import DDC.Code.Config
import DDC.Build.Builder
import System.FilePath
import qualified DDC.Driver.Stage               as D


-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Display the version string.
        | ModeVersion

        -- | Display the help page.
        | ModeHelp

        -- | Parse and type-check a module.
        | ModeCheck     FilePath

        -- | Parse, type-check and transform a module.
        | ModeLoad      FilePath

        -- | Compile a .dcl or .dce into an object file.
        | ModeCompile   FilePath

        -- | Compile a .dcl or .dce into an executable file.
        | ModeMake      FilePath

        -- | Pretty print a module's AST.
        | ModeAST       FilePath

        -- | Convert a module to Salt.
        | ModeToSalt    FilePath

        -- | Convert a module to C.
        | ModeToC       FilePath

        -- | Convert a module to LLVM.
        | ModeToLLVM    FilePath

        -- Disciple Core Flow specific ----------
        -- | Prepare a Flow program for lowering.
        | ModeFlowPrep  FilePath

        -- | Lower a Flow program.
        | ModeFlowLower FilePath

        -- | Concretize rate type variables in a Flow program.
        | ModeFlowConcretize FilePath

        -- | Thread the World token through a Flow program.
        | ModeFlowThread FilePath

        -- Builder ------------------------------
        -- | Build the base libraries and runtime.
        | ModeBaseBuild

        -- | Print the builder info for this platform.
        | ModePrintBuilder

        -- | Print where the runtime and base libraries are intalled.
        | ModePrintBaseDir
        deriving (Eq, Show)


data OptLevel
        -- | Don't do any optimisations.
        = OptLevel0

        -- | Do standard optimisations.
        | OptLevel1
        deriving Show


-- | DDC config.
data Config
        = Config
        { -- | The main compilation mode.
          configMode            :: Mode 

          -- Compilation --------------
          -- | Directory holding the runtime and base library code.
        , configBaseDir         :: FilePath

          -- | Redirect output to this file.
        , configOutputFile      :: Maybe FilePath

          -- | Redirect output to this directory.
        , configOutputDir       :: Maybe FilePath 

          -- | What backend to use for compilation
        , configViaBackend      :: D.ViaBackend

          -- Optimisation -------------
          -- | What optimisation levels to use
        , configOptLevelLite    :: OptLevel
        , configOptLevelSalt    :: OptLevel

          -- | Paths to modules to use as inliner templates.
        , configWithLite        :: [FilePath]
        , configWithSalt        :: [FilePath]

          -- Runtime -------------------
          -- | Default size of heap for compiled program.
        , configRuntimeHeapSize :: Integer

          -- Intermediates -------------
        , configKeepLlvmFiles   :: Bool
        , configKeepSeaFiles    :: Bool
        , configKeepAsmFiles    :: Bool

          -- Transformation ------------
          -- | String containing the transform definition to apply with
          --   the -load command. We can't parse this definition until
          --   we know what language we're dealing with.
        , configTrans           :: Maybe String

          -- | Other modules to use for inliner templates.
        , configWith            :: [FilePath]

          -- Debugging -----------------
          -- | Dump intermediate representations.
        , configDump            :: Bool 

          -- Taints --------------------
          -- | Disable type checking where possible.
        , configTaintAvoidTypeChecks :: Bool }
        deriving (Show)


-- | Default configuation.
getDefaultConfig :: IO Config
getDefaultConfig
 = do   baseDir <- locateBaseLibrary

        return $ Config
          { configMode            = ModeNone 
 
            -- Compilation --------------
          , configBaseDir         = baseDir
          , configOutputFile      = Nothing
          , configOutputDir       = Nothing
          , configViaBackend      = D.ViaLLVM
 
            -- Optimisation -------------
          , configOptLevelLite    = OptLevel0
          , configOptLevelSalt    = OptLevel0
          , configWithLite        = []
          , configWithSalt        = []
 
            -- Runtime ------------------
          , configRuntimeHeapSize = 65536
 
            -- Intermediates ------------
          , configKeepLlvmFiles   = False
          , configKeepSeaFiles    = False
          , configKeepAsmFiles    = False
 
            -- Transformation -----------
          , configTrans           = Nothing
          , configWith            = []
 
            -- Debugging ----------------
          , configDump            = False 

            -- Taints -------------------
          , configTaintAvoidTypeChecks = False }


-- | Get the builder configuation from the ddc configuration.
defaultBuilderConfig :: Config -> BuilderConfig
defaultBuilderConfig config
        = BuilderConfig
        { builderConfigBaseSrcDir = configBaseDir config 
        , builderConfigBaseLibDir = configBaseDir config </> "build" }

