
module Util.Data.BackGraph 
	( BackGraph (..)
	, BackNode  (..)
	, testBackGraph )
where

import Util.Test.Check
import Control.Monad
import Data.List
import Data.Map		(Map)
import qualified Data.Map as Map

-- A backwards work graph.
--	The nodes contain lists of parent jobs to be finished
--	before this one can be started.
--
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


-- TODO: This generates a graph with cycles..
--	Our workgraph code doesn't handle that.
instance Arbitrary (BackGraph Int) where
 arbitrary 
  = do	n :: Int	<- arbitrary
	keys		<- replicateM n arbitrary

	let makeNode k 	= do
		bs	<- replicateM n (elements keys)
		return	$ (k, BackNode (nub bs \\ [k]))

	nodes		<- mapM makeNode keys
	return	$ BackGraph $ Map.fromList nodes
