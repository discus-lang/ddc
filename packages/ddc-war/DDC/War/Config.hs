
module DDC.War.Config
	( Config       (..)
        , defaultConfig
        , defaultTestSpec
        , defaultNightlySpec)
where
import qualified DDC.War.Task.Test      as T
import qualified DDC.War.Task.Nightly   as N
import qualified BuildBox.Command.Mail  as B


-- | Configuration information read from command line arguments.
data Config
	= Config 
        { -- | Whether to emit debugging info for war.
	  configDebug		        :: Bool

          -- | Config for test mode.
        , configTest                    :: Maybe T.Spec

          -- | Config for nightly build mode.
        , configNightly                 :: Maybe N.Spec }
	deriving Show


-- | Default configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configDebug                   = False
        , configTest                    = Just defaultTestSpec
        , configNightly                 = Nothing }


defaultTestSpec :: T.Spec
defaultTestSpec
        = T.Spec
        { T.specTestDirs                = []
        , T.specWays                    = []
        , T.specThreads                 = 1
        , T.specFormatPathWidth         = 70
        , T.specInteractive             = True
        , T.specResultsFileAll          = Nothing
        , T.specResultsFileFailed       = Nothing }


defaultNightlySpec :: N.Spec
defaultNightlySpec
        = N.Spec
        { N.specLocalBuildDir           = Nothing
        , N.specRemoteSnapshotURL       = "http://code.ouroborus.net/ddc/snapshot/ddc-head-latest.tgz"
        , N.specRemoteRepoURL           = "http://code.ouroborus.net/ddc/ddc-head"
        , N.specBuildThreads            = 1
        , N.specBuildFlavour            = "distro"
        , N.specCleanup                 = False
        , N.specContinuous              = Nothing
        , N.specNow                     = False
        , N.specLogUserHost             = Just $ "overlord@deluge.ouroborus.net"
        , N.specLogRemoteDir            = Nothing
        , N.specLogRemoteURL            = Nothing
        , N.specMailer                  = Just $ B.MailerSendmail "sendmail" [] 
        , N.specMailFrom                = Just $ "DDC Buildbot <overlord@ouroborus.net>"
        , N.specMailTo                  = Just $ "disciple-cafe@googlegroups.com" }



