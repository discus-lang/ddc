
module DDC.War.Config
	( Mode         (..)
        , Config       (..)
        , defaultConfig)
where
import DDC.War.Create.Way


-- | Operation mode.
data Mode
        -- | Just run the tests from the current directory.
        = ModeTest

        -- | Download the repo.
        | ModeNightly
        deriving (Eq, Show)


-- | Configuration information read from command line arguments.
data Config
	= Config 
        { -- | Whether to emit debugging info for war.
	  configDebug		       :: Bool

          -- | Operation mode
        , configMode                   :: Mode

        -- | Whether to run in batch mode with no color and no interactive
        --      test failure resolution.
        , configBatch                  :: Bool 

	-- | Number of threads to use when running tests.
	, configThreads                :: Int 

	-- | What ways to compile the tests with.
	, configWays                   :: [Way] 

        -- | Width of reports.
	, configFormatPathWidth        :: Int 

        -- | Directories to recursively search for tests.
        , configTestDirs               :: [FilePath] 

        -- | Write all tests results fo this file.
        , configResultsFileAll         :: Maybe FilePath

        -- | Write failed test results to this file.
        , configResultsFileFailed      :: Maybe FilePath }
	deriving (Show, Eq)


-- | Default configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configDebug               = False
        , configMode                = ModeTest
        , configBatch               = False
        , configThreads             = 1
        , configWays                = []
        , configFormatPathWidth     = 70 
        , configTestDirs            = []
        , configResultsFileAll      = Nothing
        , configResultsFileFailed   = Nothing }

