
import DDC.War.Option
import DDC.War.Config
--import BuildBox.Command.Mail                    (Mailer(..))
--import BuildBox.Data.Schedule
import BuildBox.Pretty
import BuildBox
import System.Environment
import qualified DDC.War.Task.Nightly           as N
import qualified DDC.War.Task.Test              as Test


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args    <- getArgs
	config  <- parseOptions args defaultConfig
	
        case configNightly config of
         Nothing        -> mainTest    config
         Just spec      -> mainNightly spec


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


-- | Run the nightly build.
mainNightly :: N.Spec -> IO ()
mainNightly spec
 = do   result  <- runBuild "/tmp"  $ N.build spec
        case result of
         Left err       -> error    $ render $ ppr err
         Right result'  -> putStrLn $ render $ ppr result'
