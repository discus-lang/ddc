
module Config where

import Util.Options	as Options
import Util

data Config
	= Config
	{ configOptions		:: [Opt] 
	, configDebug		:: Bool
	, configThreads		:: Int 
	, configBatch		:: Bool 
	, configLogFailed	:: Maybe FilePath }
	deriving (Show, Eq)

-- | Command line options
data Opt
	= OptHelp			-- ^ Show command help.
	| OptDebug			-- ^ Emit debugging info for the war test driver.
	| OptFlagsDDC  [String]		-- ^ Extra flags to pass to DDC when compiling test files.
	| OptTestDirs  [String]		-- ^ Only run the tests in these dirs.
        | OptThreads   Int		-- ^ Use this many threads when running tests.
        | OptBatch			-- ^ Don't interactively ask what to do if a test fails.
	| OptLogFailed String		-- ^ Log failed tests to this file
	deriving (Show, Eq)

warOptions :: [Option Opt]
warOptions
 = 	[ ODefault	OptTestDirs
 
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
			"Log failed tests to this file"
	]


-- | Parse some command line options
parseOptions :: [String] -> ([String], [Opt])
parseOptions args
	= Options.munch warOptions
	$ Options.tokenise
	$ catInt " " args
