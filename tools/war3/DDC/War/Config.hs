
module DDC.War.Config
	( Config(..)
	, loadConfig)
where
import DDC.War.Options
import DDC.War.Way
import Util

-- | Configuration information read from command line arguments.
data Config
	= Config {
 	-- | Raw options list passed to war.
 	  configOptions		:: [Opt] 

	-- | Whether to emit debugging info for war.
	, configDebug		:: Bool

	-- | Number of threads to use when running tests.
	, configThreads		:: Int 

	-- | Whether to run in batch mode with no color and no interactive
	--	test failure resolution.
	, configBatch		:: Bool 

	-- | Where to write the list of failed tests to.
	, configLogFailed	:: Maybe FilePath 

	-- | What ways to compile the tests with.
	, configWays		:: [Way] 

	-- | Clean up ddc generated files after each test
	, configClean		:: Bool 
	
	-- | Width of reports.
	, configFormatPathWidth	:: Int }
	deriving (Show, Eq)


-- | Load command line arguments into a `Config`.
loadConfig :: [Opt] -> Config
loadConfig options
 = let
	-- Calculate all the ways we should run the tests
	--	If no options are given for comp or run, then just use
	--	a "normal" way with no options.
	makeWayPair (name:opts)	= (name, opts)
	makeWayPair way		= error $ "bad way specification " ++ catInt " " way

	compWayPairs_		= [makeWayPair opts | OptCompWay opts	<- options ]
	compWayPairs		= if null compWayPairs_ 
					then [("normal", [])] 
					else compWayPairs_

	runWayPairs_		= [makeWayPair opts | OptRunWay  opts	<- options ]
	runWayPairs		= if null runWayPairs_ 
					then [("normal", [])]
					else runWayPairs_

	ways			= [ WayOpts (compName ++ "-" ++ runName) compOpts runOpts
					| (compName, compOpts)	<- compWayPairs
					, (runName,  runOpts)	<- runWayPairs ]

    in	Config
	{ configOptions		= options
	, configDebug		= elem OptDebug options
	, configThreads		= fromMaybe 1 (takeLast [n | OptThreads n <- options])
	, configBatch		= elem OptBatch options 
	, configLogFailed	= takeLast [s | OptLogFailed s		<- options]
	, configWays		= ways
	, configClean		= elem OptClean options
	, configFormatPathWidth	= 80
	}

