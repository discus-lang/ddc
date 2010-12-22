
import Data.Char
import Data.List
import System.Exit

main :: IO ()
main
 = do	text <- readFile "test/18-ImportExport/T210-CtorOrder/A.di"
	case extract text of
	  "ABC"	-> putStrLn "Ok"
	  str	-> do { putStrLn str ; exitFailure }


extract :: String -> String
extract text
 = filter isAlpha
	$ dropWhile (/= '=')
	$ intercalate " "
	$ filter (\s -> length s > 0)
	$ filter (\s -> not (isPrefixOf "--" s))
	$ lines text
