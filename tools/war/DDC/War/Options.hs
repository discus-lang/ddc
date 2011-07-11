
module DDC.War.Options
	( Opt(..)
	, warOptions)
where
import Util.Options.Option

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
 			[ "-h", "-help", "--help"]
			"Display this help."

	, OFlag		OptDebug
			[ "-d", "-debug"]
			"Emit debugging info for the test driver."

        , OOpt		(OptThreads . read)
			[ "-j" ]
			"-j <n threads>"
			"Run n tests in parallel."

	, OFlag		OptBatch
			[ "-b", "-batch" ]
			"Don't interactively ask what to do if a test fails." 

	, OOpt		OptLogFailed
			[ "-logFailed" ]
			"-logFailed <file>"
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