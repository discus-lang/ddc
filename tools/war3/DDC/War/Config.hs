
module DDC.War.Config
	(Config(..))
where
import DDC.War.Options
import DDC.War.Way

data Config
	= Config

	-- Raw options list passed to war.
	{ configOptions		:: [Opt] 

	-- Whether to emit debugging info for war.
	, configDebug		:: Bool

	-- Number of threads to use when running tests.
	, configThreads		:: Int 

	-- Whether to run in batch mode with no color and no interactive
	--	test failure resolution.
	, configBatch		:: Bool 

	-- Where to write the list of failed tests to.
	, configLogFailed	:: Maybe FilePath 

	-- What ways to compile the tests with.
	, configWays		:: [Way] 

	-- Clean up ddc generated files after each test
	, configClean		:: Bool }
	deriving (Show, Eq)

