
module Util.Test.Check 
	( module Test.QuickCheck
	, checkTest, checkTests
	, testBool,  testBool2
	, testProp,  testProp2)
where

import Test.QuickCheck

data Test
	= forall a. (Arbitrary a, Show a)
		=> TestBool (a -> Property)

	| forall a b. (Arbitrary a, Show a, Arbitrary b, Show b)
	 	=> TestBool2 (a -> b -> Property)
	

-- | Check some named properties
checkTest :: (String, Test) -> IO ()
checkTest (name, test)
 = do	putStr $ " * " ++ name ++ replicate (40 - length name) ' ' ++ " "
	case test of
		TestBool  fun	-> quickCheck fun
		TestBool2 fun 	-> quickCheck fun
	
-- | Run some named tests
checkTests props
	= mapM_ checkTest props

testBool str fun
	= (str, TestBool  $ \x -> True ==> fun x)
	
testBool2 str fun
	= (str, TestBool2 $ \x y -> True ==> fun x y)

testProp str fun
	= (str, TestBool fun)
	
testProp2 str fun
	= (str, TestBool2 fun)
	