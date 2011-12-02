
module Data.BackGraph 
	( BackGraph (..)
	, BackNode  (..)
	, testBackGraph )
where
import Util.Test.Check
import Control.Monad
import Data.List
import Data.Map		(Map)
import qualified Data.Map as Map


-- | A backwards work graph.
--	The nodes contain lists of parent jobs to be finished
--	before this one can be started.
data BackGraph k  
	= BackGraph (Map k (BackNode k))
	deriving Show

data BackNode k
	= BackNode [k] 	
	deriving (Eq, Show)

testBackGraph :: BackGraph Int
testBackGraph
 =  BackGraph
 $  Map.fromList
	[ (0, BackNode [])
	, (1, BackNode [])
	, (2, BackNode [0])
	, (3, BackNode [0])
	, (4, BackNode [0, 1])
	, (5, BackNode [1])
	, (6, BackNode [2])
	, (7, BackNode [2])
	, (8, BackNode [4])
	, (9, BackNode [4]) ]


-- Generate an arbitrary graph that is guaranteed to be acyclic.
--
-- Keys are strickly ordered [0..n] and all BackNodes can only link to higher
-- numbered nodes thus guaranting no cycles. Now you may think these graphs
-- are no longer representative of real graphs, but you'd be wrong. All we're
-- doing here is generating a graph where the nodes are sorted in topological
-- order. Any real acyclic graph can have its nodes reordered so that is has
-- this property.
--
instance Arbitrary (BackGraph Int) where
 arbitrary 
  = do	rand :: Int	<- arbitrary
  	let n		= (abs rand) `mod` 30
	let keys	= [0 .. (n * n)]

	let makeNode k 	= do
		bs	<- replicateM 8 (elements keys)
		return	(k, BackNode $ filter (\x -> x > k) $ nub bs)

	nodes		<- mapM makeNode keys
	return	$! BackGraph $ Map.fromList nodes
