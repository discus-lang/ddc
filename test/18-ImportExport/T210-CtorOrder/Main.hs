
import Data.Char
import Data.List
import Data.List.Split
import System.Exit

-- Data constructors are given tag values (0, 1, ..) based on the order of their
-- definition so that for somehting like:
--
--     data Bool = False | True
-- the False constructor will get a tag of 0 and True will get 1.
--
-- Complications arise with interactions between the parser and the module
-- exporter. The module exporter has to export constructors in the same order
-- as they were initially read so the that when the parser reads the interface
-- file they will be in the same order as they were when the original source
-- file was read.
--
-- Below is a rough hack to make sure this is all correct.


main :: IO ()
main
 = do	testCtorOrder "test/18-ImportExport/T210-CtorOrder/A.di" "X" "ABC"
	testCtorOrder "library/Base.di" "Bool" "FalseTrue"
	testCtorOrder "library/Data/List.di" "List" "NilCons"


testCtorOrder :: FilePath -> String -> String -> IO ()
testCtorOrder fpath dataName expected
 = do	text <- readFile fpath
	let ctors = extractCtors dataName text
	if ctors == expected
	  then return ()
	  else
	    do	putStrLn $ "Oops, '" ++ ctors ++ "' should have been '" ++ expected ++ "'."
		-- exitFailure


extractCtors :: String -> String -> String
extractCtors dataName text
 = filter isAlpha
	$ dropParen
	$ dropWhile (/= '=')
	$ takeWhile (/= ';')
	$ intercalate " "
	$ dropWhile (\s -> not (isPrefixOf ("data " ++ dataName) s))
	$ filter (\s -> length s > 0)
	$ filter (\s -> not (isPrefixOf "--" s))
	$ lines text


dropParen :: String -> String
dropParen s
 = case splitOnLeft '{' s of
	([], []) -> []
	(l, []) -> l
	(l, r) -> l ++ dropParen (snd $ splitOnLeft '}' r)
