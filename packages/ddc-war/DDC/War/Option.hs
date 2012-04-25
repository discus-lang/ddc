
module DDC.War.Option
        (parseOptions)
where
import DDC.War.Create.Way
import DDC.War.Config
import Data.Maybe
import Data.Char


parseOptions :: [String] -> Config -> Config
parseOptions args0 config0
 = eat args0 config0

 where
  eat [] config = config
  eat args@(arg : rest) config
        | elem arg ["-help", "--help"]
        = printUsage Nothing

        | elem arg ["-nightly"]
        = eat rest $ config { configMode  = ModeNightly }

        | elem arg ["-d", "-debug"]
        = eat rest $ config { configDebug = True }

        | elem arg ["-b", "-batch"]
        = eat rest $ config { configBatch = True }

        | "-j" : sThreads : more     <- args
        , all isDigit sThreads
        = eat more $ config { configThreads   = read sThreads}

        | "-results" : file : more <- args
        = eat more $ config { configResultsFileAll    = Just file }

        | "-results-failed" : file : more <- args
        = eat more $ config { configResultsFileFailed = Just file }

        | "+compway" : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eat more $ config { configWays = configWays config ++ [Way name wayFlags []] }

        | "+runway"  : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eat more $ config { configWays = configWays config ++ [Way name [] wayFlags] }

        | '-' : _       <- arg
        = printUsage (Just arg)

        | otherwise
        = eat rest $ config { configTestDirs  = configTestDirs config ++ [arg]}

  printUsage badArg
   = error $ unlines
        [ "invalid argument " ++ fromMaybe "" badArg
        , " Usage: war [flags]"
        , "  -help                      Display this help"
        , "  -debug, -d                 Emit debugging info for the war test driver"
        , "  -batch, -b                 Don't interactively ask what to do if a test fails"
        , "  -j <INT>                   Set number of threads (jobs) to use." 
        , "  -results        <FILE>     Log test results to this file."
        , "  -results-failed <FILE>     Log failed tests to this file."
        , "  +compway <NAME> [OPTIONS]  Also compile with these DDC options."
        , "  +runway  <NAME> [OPTIONS]  Also run executables with these options."
        ]

