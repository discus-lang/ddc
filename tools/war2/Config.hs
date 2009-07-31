
module Config where

import Util.Options		as Options
import Util.Options.Option	as Options
import Util

-- | A way to build the test
--	This holds extra options to pass to the program.
data Way
	= WayNil
	| WayOpts 
		{ wayName	:: String 
		, wayOptsComp	:: [String] 
		, wayOptsRun	:: [String] }
	deriving (Eq, Ord, Show)

pprWayName :: Way -> String
pprWayName way
 = case way of
	WayNil		-> ""
	WayOpts{}	-> wayName way


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


-- | Command line options
data Opt
	= OptHelp			-- ^ Show command help.
	| OptDebug			-- ^ Emit debugging info for the war test driver.
	| OptFlagsDDC  [String]		-- ^ Extra flags to pass to DDC when compiling test files.
	| OptTestDir   String		-- ^ Only run the tests in these dirs.
        | OptThreads   Int		-- ^ Use this many threads when running tests.
        | OptBatch			-- ^ Don't interactively ask what to do if a test fails.
	| OptLogFailed String		-- ^ Log failed tests to this file
	| OptCompWay   [String]		-- ^ Flags to compile tests with
	| OptRunWay    [String]		-- ^ Flags to run tests with 
	| OptClean			-- ^ Cleanup ddc generated files after each test
	deriving (Show, Eq)


-- | Options accepted by the War test driver
warOptions :: [Option Opt]
warOptions
 = 	[ ODefault	OptTestDir
 
	, OFlag		OptHelp
 			[ "-h", "--help"]
			"Display this help."

	, OFlag		OptDebug
			[ "-d", "--debug"]
			"Emit debugging info for the test driver."

	, OOpts		(\ss -> OptFlagsDDC $ map ('-' :) ss)
			[ "--ddc"]
			"--ddc <options>"
			"Compile tests with these DDC options."

        , OOpt		(OptThreads . read)
			[ "-j" ]
			"-j <n threads>"
			"Run n tests in parallel."

	, OFlag		OptBatch
			[ "-b", "--batch" ]
			"Don't interactively ask what to do if a test fails." 

	, OOpt		OptLogFailed
			[ "--logFailed" ]
			"--logFailed <file>"
			"Log failed tests to this file."

	, OOptEscape	OptCompWay
			[ "+compway" ]
			"+compway <name> <options..>"
			"Compile tests with these options."

	, OOptEscape	OptRunWay
			[ "+runway" ]
			"+runway <name> <options..>"
			"Run test binaries with these options."

	, OFlag		OptClean
			[ "-clean" ]
			"Cleanup after each test"
	]

