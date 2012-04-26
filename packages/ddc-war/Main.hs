
import DDC.War.Option
import DDC.War.Config
import BuildBox.Pretty
import BuildBox.Command.Mail                    (Mailer(..))
import BuildBox
import System.Environment
import qualified DDC.War.Task.Nightly           as Nightly
import qualified DDC.War.Task.Test              as Test


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args           <- getArgs
	let config      = parseOptions args defaultConfig
	
        case configMode config of
         ModeNightly    -> mainNightly config
         ModeTest       -> mainTest    config


-- | Run the nightly build.
mainNightly :: Config -> IO ()
mainNightly _config
 = let spec
         = Nightly.Spec
         { Nightly.specRemoteSnapshotURL = "http://code.ouroborus.net/ddc/snapshot/ddc-head-latest.tgz"
         , Nightly.specRemoteRepoURL     = "http://code.ouroborus.net/ddc/ddc-head"
         , Nightly.specLocalBuildDir     = "." 
         , Nightly.specRelPackageDir     = "ddc-head" 
         , Nightly.specBuildThreads      = 4 
         , Nightly.specMailer            = Just $ MailerSendmail "sendmail" [] 
         , Nightly.specMailFrom          = "DDC Buildbot <overlord@ouroborus.net>"
         , Nightly.specMailTo            = "benl@ouroborus.net" }

   in do
        result  <- runBuild "/tmp" $ Nightly.build spec
        case result of
         Left err       -> error    $ render $ ppr err
         Right result'  -> putStrLn $ render $ ppr result'


-- | Run tests from the provided directories
mainTest :: Config -> IO ()
mainTest config
 = let spec
        = Test.Spec
        { Test.specTestDirs             = configTestDirs config
        , Test.specWays                 = configWays     config
        , Test.specThreads              = configThreads  config
        , Test.specFormatPathWidth      = configFormatPathWidth   config
        , Test.specInteractive          = not $ configBatch       config 
        , Test.specResultsFileAll       = configResultsFileAll    config
        , Test.specResultsFileFailed    = configResultsFileFailed config }
   in do
        result  <- runBuild "/tmp" $ Test.build spec
        case result of
         Left err       -> error    $ render $ ppr err
         Right _        -> return ()



