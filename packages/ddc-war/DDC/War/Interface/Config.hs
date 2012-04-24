
module DDC.War.Interface.Config
	( Mode         (..)
        , Way          (..)
        , Config       (..)
        , defaultConfig)
where


-- | Operation mode.
data Mode
        -- | Just run the tests from the current directory.
        = ModeTest

        -- | Download the repo.
        | ModeNightly
        deriving (Eq, Show)


-- | A way to build the test.
--   This holds extra options to pass to the program.
data Way
        = Way   
        { wayName       :: String 
        , wayOptsComp   :: [String] 
        , wayOptsRun    :: [String] }
        deriving (Eq, Ord, Show)


-- | Configuration information read from command line arguments.
data Config
	= Config 
        { -- | Whether to emit debugging info for war.
	  configDebug		:: Bool

          -- | Operation mode
        , configMode            :: Mode

        -- | Whether to run in batch mode with no color and no interactive
        --      test failure resolution.
        , configBatch           :: Bool 

        -- | Clean up ddc generated files after each test
        , configClean           :: Bool 

	-- | Number of threads to use when running tests.
	, configThreads		:: Int 

	-- | Where to write the list of failed tests to.
	, configLogFailed	:: Maybe FilePath 

	-- | What ways to compile the tests with.
	, configWays		:: [Way] 

        -- | Width of reports.
	, configFormatPathWidth	:: Int 

        -- | Directories to recursively search for tests.
        , configTestDirs       :: [FilePath] }
	deriving (Show, Eq)


-- | Default configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configDebug           = False
        , configMode            = ModeTest
        , configBatch           = False
        , configClean           = False
        , configThreads         = 1
        , configLogFailed       = Nothing
        , configWays            = []
        , configFormatPathWidth = 80 
        , configTestDirs        = [] }

