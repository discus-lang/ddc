
module DDC.War.Config
	( Config       (..)
        , defaultConfig
        , defaultNightly)
where
import DDC.War.Create.Way
import qualified DDC.War.Task.Nightly   as N
import qualified BuildBox.Command.Mail  as B


-- | Configuration information read from command line arguments.
data Config
	= Config 
        { -- | Whether to emit debugging info for war.
	  configDebug		       :: Bool

        -- | Whether to run in batch mode with no color and no interactive
        --      test failure resolution.
        , configBatch                  :: Bool 

	-- | Number of threads to use when running tests.
	, configThreads                :: Int 

	-- | What ways to compile the tests with.
	, configWays                   :: [Way] 

        -- | Width of reports.
	, configFormatPathWidth        :: Int 

        -- | Directories to recursively search for tests.
        , configTestDirs               :: [FilePath] 

        -- | Write all tests results fo this file.
        , configResultsFileAll         :: Maybe FilePath

        -- | Write failed test results to this file.
        , configResultsFileFailed      :: Maybe FilePath

        -- | Config for nightly build mode
        , configNightly                 :: Maybe N.Spec }
	deriving Show


-- | Default configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configDebug                   = False
        , configBatch                   = False
        , configThreads                 = 1
        , configWays                    = []
        , configFormatPathWidth         = 70 
        , configTestDirs                = []
        , configResultsFileAll          = Nothing
        , configResultsFileFailed       = Nothing 
        , configNightly                 = Nothing }


defaultNightly :: N.Spec
defaultNightly
        = N.Spec
        { N.specLocalBuildDir     = Nothing
        , N.specRemoteSnapshotURL = "http://code.ouroborus.net/ddc/snapshot/ddc-head-latest.tgz"
        , N.specRemoteRepoURL     = "http://code.ouroborus.net/ddc/ddc-head"
        , N.specBuildThreads      = 1

        , N.specContinuous        = Nothing
        , N.specNow               = False

        , N.specLogUserHost       = Just $ "overlord@deluge.ouroborus.net"
        , N.specLogRemoteDir      = Just $ "log/desire/ddc/head"
        , N.specLogRemoteURL      = Just $ "http://log.ouroborus.net/desire/ddc/head"

        , N.specMailer            = Just $ B.MailerSendmail "sendmail" [] 
        , N.specMailFrom          = Just $ "DDC Buildbot <overlord@ouroborus.net>"
        , N.specMailTo            = Just $ "benl@ouroborus.net" }



