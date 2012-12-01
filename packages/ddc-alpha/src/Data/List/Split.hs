{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module Data.List.Split
	( test_UtilDataListSplit
	, splitWhenLeft,  chopWhenLeft
	, splitOnLeft,    chopOnLeft
	, splitWhenRight, chopWhenRight
	, splitOnRight,   chopOnRight
	, breakOns
	, breakWhens)	
where
import Util.Test.Check
import Data.List

test_UtilDataListSplit
 = 	[ test_chopOnLeft_inv
	, test_chopOnRight_inv
	, test_breakOns_inv ]


-- Split Left --------
-- | Split a list on the left of the first element where this predicate matches.
splitWhenLeft ::	(a -> Bool) -> [a]	-> ([a], [a])
splitWhenLeft	p	xx	= splitWhenLeft' p xx []

splitWhenLeft'	_ []	 acc	= (acc, [])
splitWhenLeft'	p (x:xs) acc
	| p x			= (acc, x : xs)
	| otherwise		= splitWhenLeft' p xs (acc ++ [x])

chopWhenLeft :: (a -> Bool) -> [a] -> [[a]]
chopWhenLeft f xx	= makeSplits (splitWhenLeft f) xx

-- | Split a list on the left of the first occurance of this element.
--	eg splitLeft '.' "abc.def"  => ("abc", ".def")

splitOnLeft :: Eq a => a -> [a] -> ([a], [a])
splitOnLeft c xx	= splitWhenLeft (== c) xx

-- | Split a list on the left of all occurances of this element.
--	eg chopBefore '.' "abc.def.ghi.jkl"	=> ["abc", ".def", ".ghi", ".jkl"]
chopOnLeft :: Eq a => a -> [a] -> [[a]]
chopOnLeft c xx		= makeSplits (splitOnLeft c) xx

-- @ chopOnLeft is invertable
test_chopOnLeft_inv
	= testBool "chopOnLeft_inv"
	$  \(n :: Int) (xx :: [Int])
	-> xx == concat (chopOnLeft (xx !! mod n (length xx)) xx)


-- Split Right ---------
-- | Split a list on the right of the first element where this predicate matches
--
splitWhenRight ::	(a -> Bool) -> [a]	-> ([a], [a])
splitWhenRight	p	xx		= splitWhenRight' p xx []

splitWhenRight'	_	[]	acc	= (acc, [])
splitWhenRight'	p	(x:xs)	acc
	| p x				= (acc ++ [x], xs)
	| otherwise			= splitWhenRight' p xs (acc ++ [x])

chopWhenRight :: (a -> Bool) -> [a] -> [[a]]
chopWhenRight f	xx	= makeSplits (splitWhenRight f) xx


-- | Split a list on the right of the first occurance of this element.
--	eg splitRight '.' "abc.def" => ("abc.", "def")
splitOnRight :: Eq a => a -> [a] -> ([a], [a])
splitOnRight c xx	= splitWhenRight (== c) xx


-- | Split a list on the right of all occurances of this element.
--	eg chopRight '.' "abc.def.ghi.jkl" 	=> ["abc.", "def.", "ghi.", "jkl"]
chopOnRight :: Eq a => a -> [a] -> [[a]]
chopOnRight c xx	= makeSplits (splitOnRight c) xx

-- @ chopOnRight is invertable
test_chopOnRight_inv
	= testBool "chopOnRight_inv"
	$  \(n :: Int) (xx :: [Int]) 
	-> xx == concat (chopOnRight (xx !! mod n (length xx)) xx)
	
-----
-- Split Functions
--
makeSplits 
	:: ([a] -> ([a], [a])) 
	-> ([a] -> [[a]])

makeSplits splitFunc xx
	| []	<- back
	= [front]

	| []		<- front
	, (x:xs)	<- xx
	= [] ++ appendFront x (makeSplits splitFunc xs)
	
	| otherwise
	= front : makeSplits splitFunc back
	
	where 	(front, back) 		= splitFunc xx
		appendFront z (x:xs)	= (z : x) : xs
		appendFront _ []	= error "makeSplits: empty list"
		


-- | Make a breaks function from this split function
makeBreaks
	:: ([a] -> ([a], [a])) -> ([a] -> [[a]])

makeBreaks	splitFunc xx
 = let 	parts 		= makeSplits splitFunc xx
	firstParts'	= map init $ init parts
   in 	firstParts' ++ [last parts]
	

-- | Break a list into components, using this element as the separator.
--	The element is not returned as part of the components.
--
--	eg:  breakOns '.' "rabbit.marmot.lemur"   
--	  -> ["rabbit", "marmot", "lemur"]
--
breakOns :: Eq a => a -> [a] -> [[a]]
breakOns  c	= makeBreaks (splitOnRight c)

-- @ breakOns is invertable
test_breakOns_inv
	= testBool "breakOns_inv"
	$ \(n :: Int) (xx :: [Int])
	-> xx ==	let nx = xx !! (mod n (length xx))
			in concat (intersperse [nx] $ breakOns nx xx)


-- | Break a list into comonents, using this function to choose the separator.
--	The separator element is not returned as part of the components.
--
--	eg: breakWhens (isEven) [1, 5, 2, 5, 9, 5, 7]
--	 -> [[1, 5], [5, 9], [5, 7]]
--
breakWhens :: (a -> Bool) -> [a] -> [[a]]
breakWhens p	= makeBreaks (splitWhenRight p)

	
