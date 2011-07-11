
module TestNode 
	( TestId
	, TestNode(..)
	, testOfNode
	, testIdOfNode
	, node1
	, expandWays
	, chainTests
	, chainNodes)
where
import Config
import Test
import Data.ListUtil


-- | An identifier for a way'd test.
type TestId	= (Test, Way)

data TestNode	
	= TestNode 
		TestId 		-- id of this test
		[TestId]	-- tests that need to complete before this one
	deriving Show

-- Make a single node with these deps
node1 :: Test -> [Test] -> TestNode
node1 test deps
	= TestNode (test, WayNil) [(d, WayNil) | d <- deps ]


-- Get the test from this node's id.
testOfNode :: TestNode -> Test
testOfNode (TestNode (t, way) _)	= t

testIdOfNode :: TestNode -> TestId
testIdOfNode (TestNode tid _)		= tid


-- | Expand these testnodes to run all these ways, in order.
expandWays :: [Way] -> [TestNode] -> [TestNode]
expandWays ways nodes
 = let	newNodes	= map (\w -> map (patchWayOfTestNode w) nodes) ways
   in	concat $ chainNodes newNodes


-- Chain a list of tests so the last one can only be run after
--	the previous one finishes. 
--	This creates BackNodes which contain the required dependencies.
--	The Way is set to WayNil
--
chainTests :: [Test] -> [TestNode]
chainTests  (t1:rest)		
	= TestNode (t1, WayNil) []
	: chainTests' t1 rest

chainTests' tPrev []		
	= []

chainTests' tPrev (t:ts)	
	= TestNode (t, WayNil) [(tPrev, WayNil)]
	: chainTests' tPrev ts
	
	
-- Chain a list of lists of nodes, so the first node
--	of each group depends on the last node of the previous one.
chainNodes :: [[TestNode]] -> [[TestNode]]
chainNodes []		= []
chainNodes (x : [])	= x : []

chainNodes ([] : y : rest)
	= [] : chainNodes (y : rest)

chainNodes (x  : [] : rest)
	= x : chainNodes ([] : rest)

chainNodes (x : y : rest)
 = let	Just (TestNode xTest _)		= takeLast x

	Just (TestNode yTest yBack)	= takeHead y
	Just yTail			= takeTail y

	yHead'				= TestNode yTest (xTest : yBack)
	y'				= yHead' : yTail

   in 	x : chainNodes (y' : rest)

	
patchWayOfTestNode :: Way -> TestNode -> TestNode
patchWayOfTestNode way (TestNode (test, _) testWays)
 = 	TestNode 
		(test, way)
		(map (\(t, _) -> (t, way)) testWays)
		

