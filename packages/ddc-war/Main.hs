
import DDC.War.Option
import DDC.War.Config
import BuildBox.Pretty
import BuildBox
import System.Environment
import qualified DDC.War.Task.Nightly           as N
import qualified DDC.War.Task.Test              as T


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
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
 = let 
   in do
        result  <- runBuild "/tmp" $ T.build spec
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
