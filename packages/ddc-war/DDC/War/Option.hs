
module DDC.War.Option
        (parseOptions)
where
import DDC.War.Create.Way
import DDC.War.Config
import Data.Char
import DDC.War.Task.Test                as T
import DDC.War.Task.Nightly             as N
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

        | "-nightly" : dir : more     <- args
        = case eatn more defaultNightlySpec of
           Left  badArg  -> error $ "Invalid argument " ++ show badArg
           Right nspec   -> config { configNightly = Just (nspec { specLocalBuildDir = Just dir }) }

        | otherwise
        = case eatt args defaultTestSpec of
           Left  badArg  -> error $ "Invalid argument " ++ show badArg
           Right tspec   -> config { configTest    = Just tspec }


  -- Parse options for test mode
  eatt [] spec = Right spec
  eatt args@(arg : rest) spec
        | elem arg ["-b", "-batch"]
        = eatt rest $ spec { T.specInteractive = False }

        | "-j" : sThreads : more     <- args
        , all isDigit sThreads
        = eatt more $ spec { T.specThreads       = read sThreads}

        | "-results" : file : more <- args
        = eatt more $ spec { T.specResultsFileAll    = Just file }

        | "-results-failed" : file : more <- args
        = eatt more $ spec { T.specResultsFileFailed = Just file }

        | "+compway" : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eatt more $ spec { T.specWays = T.specWays spec ++ [Way name wayFlags []] }

        | "+runway"  : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eatt more $ spec { T.specWays = T.specWays spec ++ [Way name [] wayFlags] }

        | '-' : _       <- arg
        = Left arg

        -- Accept dirs for test mode
        | otherwise
        = eatt rest  $ spec { specTestDirs  = specTestDirs spec ++ [arg]}


  -- Parse options for nightly mode.
  eatn [] spec = Right spec
  eatn args@(_arg:_rest) spec
        | "-build-dir" : dir : more     <- args
        = eatn more $ spec { N.specLocalBuildDir = Just dir }

        | "-daily" : time : more   <- args
        = eatn more $ spec { N.specContinuous = Just $ B.Daily $ read time }

        | "-now" : more                 <- args
        = eatn more $ spec { N.specNow = True }

        | "-sendmail" : more            <- args
        = eatn more $ spec { N.specMailer   = Just $ B.MailerSendmail "sendmail" [] }

        | "-msmtp" : port : more        <- args
        = eatn more $ spec { N.specMailer   = Just $ B.MailerMSMTP "msmtp" (Just $ read port) }

        | "-mail-from" : addr : more    <- args
        = eatn more $ spec { N.specMailFrom = Just addr }

        | "-mail-to" : addr : more      <- args
        = eatn more $ spec { N.specMailTo   = Just addr }

        | "-log-userhost" : str : more  <- args
        = eatn more $ spec { N.specLogUserHost = Just str }

        | "-log-remote-dir" : dir : more <- args
        = eatn more $ spec { N.specLogRemoteDir = Just dir }

        | "-log-remote-url" : url : more <- args
        = eatn more $ spec { N.specLogRemoteURL = Just url }

        | "-build-threads" : threads : more <- args
        , all isDigit threads
        , t     <- read threads
        , t > 0
        = eatn more $ spec { N.specBuildThreads = t}

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

