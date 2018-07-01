{-# LANGUAGE CPP #-}

-- | Define the command line configuation arguments.
module DDC.Main.Config
        ( Mode                (..)
        , OptLevel            (..)
        , D.ViaBackend        (..)
        , D.RuntimeLinkStrategy (..)
        , Config              (..)

        , getDefaultConfig
        , defaultBuilderConfig)
where
import DDC.Build.Builder
import qualified DDC.Driver.Stage               as D


-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Display the version string.
        | ModeVersion

        -- | Display the help page.
        | ModeHelp

        -- | Scan a module, producing tokens.
        --   The flag says whether to produce source locations.
        | ModeScan      FilePath Bool

        -- | Parse a module.
        | ModeParse     FilePath

        -- | Parse and type-check a module.
        | ModeCheck     FilePath

        -- | Parse, type-check and transform a module.
        | ModeLoad      FilePath

        -- | Compile source code into an object code.
        | ModeCompile   FilePath

        -- | Compile source code into an executable.
        | ModeMake      [FilePath]

        -- | Build libraries or executables following a build .spec file.
        | ModeBuild     FilePath

        -- | Become a language server, using the LSP protocol.
        | ModeLSP

        -- | Convert a module to Salt.
        | ModeToSalt    FilePath

        -- | Convert a module to LLVM.
        | ModeToLLVM    FilePath

        -- Disciple Core Flow specific ----------
        -- | Prepare a Flow program for lowering.
        | ModeFlowPrep          FilePath

        -- | Lower a Flow program.
        | ModeFlowLower         FilePath

        -- | Lower a Flow program, producing a vector kernel.
        | ModeFlowLowerKernel   FilePath

        -- | Lower a Flow program using vector instructions.
        | ModeFlowLowerVector   FilePath

        -- | Wind loop primops into tail recursive loops.
        | ModeFlowWind          FilePath

        -- | Concretize rate type variables in a Flow program.
        | ModeFlowConcretize    FilePath

        -- | Melt compound data types in a Flow program.
        | ModeFlowMelt          FilePath

        -- | Thread the World token through a Flow program.
        | ModeFlowThread        FilePath


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

          -- Language -----------------
          -- | Infer type annotations for Disciple Core files.
        , configInferTypes      :: Bool

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
        , configOptLevelSalt    :: OptLevel

          -- | Paths to modules to use as inliner templates.
        , configWithSalt        :: [FilePath]

          -- Runtime -------------------
          -- | Default size of heap for compiled program.
        , configRuntimeHeapSize      :: Integer

          -- | Strategy for linking the runtime.
        , configRuntimeLinkStrategy  :: D.RuntimeLinkStrategy

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

          -- Language Server -----------
          -- | Write debugging information for the language server to the given file.
        , configLspLogDebug     :: Maybe FilePath

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
 = do   let baseDir = "src/s2"

        return $ Config
          { configMode                  = ModeNone

            -- Language -----------------
          , configInferTypes            = True

            -- Compilation --------------
          , configBaseDir               = baseDir
          , configOutputFile            = Nothing
          , configOutputDir             = Nothing
          , configViaBackend            = D.ViaLLVM

            -- Optimisation -------------
          , configOptLevelSalt          = OptLevel0
          , configWithSalt              = []

            -- Runtime ------------------
          , configRuntimeHeapSize       = 64 * 1024
          , configRuntimeLinkStrategy   = D.LinkDefault

            -- Intermediates ------------
          , configKeepLlvmFiles         = False
          , configKeepSeaFiles          = False
          , configKeepAsmFiles          = False

            -- Transformation -----------
          , configTrans                 = Nothing
          , configWith                  = []

            -- Language Server ----------
          , configLspLogDebug           = Nothing

            -- Debugging ----------------
          , configDump                  = False

            -- Taints -------------------
          , configTaintAvoidTypeChecks  = False }


-- | Get the builder configuation from the ddc configuration.
defaultBuilderConfig :: Config -> BuilderConfig
defaultBuilderConfig config
        = BuilderConfig
        { builderConfigBaseSrcDir = configBaseDir config
        , builderConfigBaseLibDir = configBaseDir config
        , builderConfigLinkStatic = configRuntimeLinkStrategy config == D.LinkStatic }

