
import DDC.War.Options
import DDC.War.Way	()
import DDC.War.Config	()
import Util.Options
import Util.Options.Help
import System.Environment
import System.Exit
import Control.Monad
import Util


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	-- Print command usage if asked for
	when (elem OptHelp options)
	 $ do	putStrLn $ help
		exitSuccess

	-- Print errors if there are any
	when (not $ null errs)
	 $ do	putStrLn $ (catInt "\n" errs) 
		putStrLn $ help
		exitFailure