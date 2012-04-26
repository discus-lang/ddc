
module DDC.War.Option
        (parseOptions)
where
import DDC.War.Create.Way
import DDC.War.Config
import Data.Maybe
import Data.Char
import DDC.War.Task.Nightly             (Spec(..))
import qualified BuildBox.Command.Mail  as B
import qualified BuildBox.Data.Schedule as B


parseOptions :: [String] -> Config -> IO Config
parseOptions [] _       
 = do   printUsage Nothing
        error "Nothing to do..."

parseOptions args _
 | elem "-help" args || elem "--help" args
 = do   printUsage Nothing
        error "Nothing to do..."

parseOptions args0 config0
 = return $ eat args0 config0
 where
  eat [] config = config
  eat args@(arg : rest) config
        | elem arg ["-d", "-debug"]
        = eat rest $ config { configDebug = True }

        -- Test mode ----------------------------
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

        -- Nightly mode ------------------------
        | "-nightly" : dir : more     <- args
        = case eatn more defaultNightly of
           Left  badArg  -> error $ "Invalid argument " ++ show badArg
           Right nspec   -> config { configNightly = Just (nspec { specLocalBuildDir = Just dir }) }

        -- Other stuff -------------------------
        | '-' : _       <- arg
        = error $ "Invalid argument " ++ show arg

        -- Accept dirs for test mode
        | isNothing $ configNightly config
        = eat rest  $ config { configTestDirs  = configTestDirs config ++ [arg]}

        | otherwise
        = error $ "Invalid argument " ++ show arg

  -- Parse options for nightly mode.
  eatn [] spec = Right spec
  eatn args@(_arg:_rest) spec
        | "-build-dir" : dir : more     <- args
        = eatn more $ spec { specLocalBuildDir = Just dir }

        | "-daily" : time : more   <- args
        = eatn more $ spec { specContinuous = Just $ B.Daily $ read time }

        | "-now" : more                 <- args
        = eatn more $ spec { specNow = True }

        | "-sendmail" : more            <- args
        = eatn more $ spec { specMailer   = Just $ B.MailerSendmail "sendmail" [] }

        | "-msmtp" : port : more        <- args
        = eatn more $ spec { specMailer   = Just $ B.MailerMSMTP "msmtp" (Just $ read port) }

        | "-mail-from" : addr : more    <- args
        = eatn more $ spec { specMailFrom = Just addr }

        | "-mail-to" : addr : more      <- args
        = eatn more $ spec { specMailTo   = Just addr }

        | "-log-userhost" : str : more  <- args
        = eatn more $ spec { specLogUserHost = Just str }

        | "-log-remote-dir" : dir : more <- args
        = eatn more $ spec { specLogRemoteDir = Just dir }

        | "-log-remote-url" : url : more <- args
        = eatn more $ spec { specLogRemoteURL = Just url }

        | "-build-threads" : threads : more <- args
        , all isDigit threads
        , t     <- read threads
        , t > 0
        = eatn more $ spec { specBuildThreads = t}

  eatn (arg : _) _
        = Left arg

printUsage :: Maybe String -> IO ()
printUsage badArg
   = putStr 
   $ unlines
   $ maybe [] (\arg -> ["invalid argument " ++ arg]) badArg
   ++   [ "Usage: war [flags]"
        , "  -help                         Display this help."
        , "  -debug, -d                    Emit debugging info for the war test driver."
        , "  -nightly <DIR>                Become the nightly buildbot in this directory."
        , ""
        , " Test mode:     war <TESTDIR> ..."
        , "  -batch, -b                    Don't interactively ask what to do if a test fails."
        , "  -j <INT>                      Set number of threads (jobs) to use." 
        , "  -results        <FILE>        Log test results to this file."
        , "  -results-failed <FILE>        Log failed tests to this file."
        , "  +compway <NAME> [OPTIONS]     Also compile with these DDC options."
        , "  +runway  <NAME> [OPTIONS]     Also run executables with these RTS options."
        , ""
        , " Buildbot mode: war -nightly <DIR> [flags] ..."
        , "  -daily          <HH:MM:SS>    Build every day at this time (in UTC)"
        , "  -now                           ... and also do a build right now."
        , ""
        , "  -sendmail                     (*) Use   sendmail to report build results"
        , "  -msmtp          <PORT>           (or) msmtp instead"
        , "  -mail-from      <ADDRESS>     (*) ... and send from this address"
        , "  -mail-to        <ADDRESS>     (*) ... to this address."
        , ""
        , "  -log-userhost <USER@HOSTNAME> (*) Use 'scp' to copy logs to this server"
        , "  -log-remote-dir <DIR>         (*) ... to this remote directory"
        , "  -log-remote-url <URL>         (*) ... with the logs appearing at this public URL."
        , ""
        , "  -ddc-snapshot   <URL>         (*) Download DDC snapshot from this URL."
        , "  -ddc-repository <URL>         (*) Update snapshot with this repository."
        , "  -build-threads  <INT>         (*) Threads to use when building DDC."
        , ""
        , "  (*) Defaults are:"
        , "   -sendmail"
        , "   -mail-from      \"DDC Buildbot <overlord@ouroborus.net>\""
        , "   -mail-to        disciple-cafe@googlegroups.com"
        , "   -log-userhost   overlord@ouroborus.net"
        , "   -log-remote-dir log/desire/ddc/head"
        , "   -log-remote-url http://log.ouroborus.net/desire/ddc/head"
        , "   -ddc-snapshot   http://code.ouroborus.net/ddc/snapshot/ddc-head-latest.tgz"
        , "   -ddc-repository http://code.ouroborus.net/ddc/ddc-head"
        , "   -build-threads   1"
        , ""
        ]

