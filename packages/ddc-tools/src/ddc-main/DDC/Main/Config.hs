
-- | Define the command line configuation arguments.
module DDC.Main.Config
        ( Mode         (..)
        , OptLevel     (..)
        , D.ViaBackend (..)
        , Config       (..)

        , defaultConfig
        , defaultBuilderConfig

        , liteBundleOfConfig
        , saltBundleOfConfig
        , bundleFromFilePath)
where
import DDC.Build.Language
import DDC.Build.Builder
import DDC.Driver.Bundle
import System.FilePath
import qualified DDC.Driver.Stage       as D
import qualified DDC.Core.Simplifier    as S
import qualified Data.Map               as Map


-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Display the help page.
        | ModeHelp

        -- | Parse and type-check a module.
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

        -- | Print the builder info for this platform.
        | ModePrintBuilder
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
          -- | Path to the base library code.
        , configLibraryPath     :: FilePath

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
        , configDump            :: Bool }
        deriving (Show)


-- | Default configuation.
defaultConfig :: Config
defaultConfig
        = Config
        { configMode            = ModeNone 

          -- Compilation --------------
        , configLibraryPath     = "code"
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
        , configDump            = False }


defaultBuilderConfig :: Config -> BuilderConfig
defaultBuilderConfig config
        = BuilderConfig
        { builderConfigRuntime  = configLibraryPath config }


-- | Get the Lite specific stuff from the config.
liteBundleOfConfig :: Config -> Bundle
liteBundleOfConfig _config
 = Bundle
        { bundleFragment        = fragmentLite
        , bundleModules         = Map.empty
        , bundleStateInit       = ()
        , bundleSimplifier      = S.Trans S.Id
        , bundleRewriteRules    = Map.empty }


-- | Get the Salt specific stuff from the config.
saltBundleOfConfig :: Config -> Bundle
saltBundleOfConfig _config
 = Bundle
        { bundleFragment        = fragmentSalt
        , bundleModules         = Map.empty
        , bundleStateInit       = ()
        , bundleSimplifier      = S.Trans S.Id
        , bundleRewriteRules    = Map.empty }


-- | Determine the current language based on the file extension of this path, 
--   and slurp out a bundle of stuff specific to that language from the config.
bundleFromFilePath :: Config -> FilePath -> Maybe Bundle
bundleFromFilePath config filePath
 = case takeExtension filePath of
        ".dcl"  -> Just (liteBundleOfConfig config)
        ".dcs"  -> Just (saltBundleOfConfig config)
        _       -> Nothing


