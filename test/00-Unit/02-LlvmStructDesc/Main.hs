
import Llvm
import Llvm.Runtime.Struct

import System.Exit			(exitFailure)
import Text.Printf

import qualified Config.Config		as Config


main :: IO ()
main
 = do	testOne
	testTwo

--------------------------------------------------------------------------------

testOne :: IO ()
testOne
 = do	putStrLn "testOne :\n"
	let abstract =
		[ AField "zero" i8
		, APadTo4
		, AField "one" i16
		, APadTo4
		, AField "two" i32
		, AField "three" i16
		, APadTo8
		, AField "four" (LMArray 0 i16) ]

	let structDesc = mkLlvmStructDesc "test" abstract

	putStrLn "    name        index   offset     type"
	putStrLn "    -----------------------------------"
	mapM_ (showField structDesc) $ reverse $ foldl getNames [] abstract

getNames :: [String] -> LlvmStructField -> [String]
getNames accum (AField name _) = name : accum
getNames accum _ = accum


showField :: LlvmStructDesc -> String -> IO ()
showField struct name
 = do	let (i, t) = structFieldLookup struct name
	let o = offsetOfIndex (llvmTypeOfStruct struct) i
	printf "    %-8s    %3d     %3d        %s\n" name i o (show t)

--------------------------------------------------------------------------------

testTwo :: IO ()
testTwo
 = do	putStr "\ntestTwo : "
	let s = mkLlvmStructDesc "test"
				[ AField "a" i32
				, APadTo8If64
				, AField "b" (LMPointer LMVoid) ]
	assertLayout s "a" 0
	assertLayout s "b" Config.pointerBytes
	putStrLn "Success\n"


assertLayout :: LlvmStructDesc -> String -> Int -> IO ()
assertLayout struct name offset
 = do	let (i, t) = structFieldLookup struct name
	let o = offsetOfIndex (llvmTypeOfStruct struct) i
	if o == offset
	  then return ()
	  else do
		printf "Failed, offsetOfIndex returned %d (should be %d).\n\n" o offset
		exitFailure
