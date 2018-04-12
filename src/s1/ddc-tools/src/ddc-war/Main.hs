
import DDC.War.Config
import DDC.War.Option
import BuildBox
import System.Environment
import System.Directory
import qualified DDC.War.Task.Nightly           as N
import qualified DDC.War.Task.Test              as T


main :: IO ()
main
 = do   -- Parse command line options, and exit if they're no good.
        args    <- getArgs
        config  <- parseOptions args defaultConfig

        case configNightly config of
         Nothing
          -> let Just spec = configTest config
             in  mainTest spec

         Just spec
          -> mainNightly  spec


-- | Run tests from the provided directories
mainTest :: T.Spec -> IO ()
mainTest spec
 = do   tmp     <- getTemporaryDirectory
        result  <- runBuild tmp $ T.build spec
        case result of
         Left err       -> error    $ show err
         Right _        -> return ()


-- | Run the nightly build.
mainNightly :: N.Spec -> IO ()
mainNightly spec
 = do   tmp     <- getTemporaryDirectory
        result  <- runBuild tmp  $ N.build spec
        case result of
         Left err       -> error    $ show err
         Right result'  -> putStrLn $ show result'
