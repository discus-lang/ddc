{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.State.Merge
	(mergeClasses)
where
import DDC.Solve.State.Base
import DDC.Solve.State.Sink
import DDC.Solve.State.Squid
import DDC.Solve.State.Graph
import DDC.Type
import DDC.Main.Pretty
import DDC.Main.Error
import Type.Error
import Data.List
import Data.Array.IO
import Control.Monad
import Control.Monad.Trans
import qualified Data.Sequence	as Seq
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util.Data.List

stage	= "DDC.Solve.Merge"
debug	= False
trace s	= when debug $ traceM s

-- | Merge several classes by combining their constraints.
--
--	This just combines the information, but doesn't perform actual unification, 
--	which is handled by the DDC.Solve.Crush.Unify.
--
--	The class with the lowest cid gets all the constraints and the others are updated
--	to be `ClassFoward`s which point to it.
mergeClasses
	:: [ClassId] 
	-> SquidM ClassId

mergeClasses []
 	= panic stage $ "mergeClasses: empty list"

-- If there is just a single cid then there's nothing to do
mergeClasses [cid_]
 = do	cid'		<- sinkClassId cid_
   	return	cid'
	
mergeClasses cids_
 = do	
	-- Sink the cids and lookup their classes.
 	cids	<- liftM nub $ mapM sinkClassId cids_
	Just cs	<- liftM sequence  $ mapM lookupClass cids
			
	-- Make sure all the classes have the same kind	
	let ks	= map (\Class { classKind } -> classKind) cs
	
	case nub ks of
	 []		-> panic stage $ "mergeClasses: empty list"
	 [_]		-> mergeClasses2 cids cs 
	 (k1:k2:_)	-> mergeClasses_kindMismatch cids cs k1 k2
	
	
mergeClasses2 cids cs
 = do	
	-- The class with the lowest cid gets all the items.
	let Just cidL	= takeMinimum cids
	Just cL		<- lookupClass cidL
	
	trace 	$ "-- mergeClasses\n"
		% "    cidL = " % cidL % "\n"
		% "    cids = " % cids % "\n"

	-- Combine information from all the classes into one.
	let cL'	= cL 	
		{ classAliases		= Map.unions $ map classAliases cs
		, classUnified		= Nothing
		, classTypeSources	= concatMap classTypeSources cs
		, classFetters		= Map.unionsWith (Seq.><) $ map classFetters cs
		, classFettersMulti	= Set.unions $ map classFettersMulti cs  }

	updateClass cidL cL'

	-- Mark the class as active so it gets visited by the unifier
	-- and other crushers.
	activateClass cidL

	-- Add forwards from old classes to new class.
	let cidsH	= cids \\ [cidL]
	addClassForwards cidL cidsH
	
  	return cidL


mergeClasses_kindMismatch [] _ _ _ 
	= panic stage $ "mergeClasses_kindMismatch: no match"

mergeClasses_kindMismatch (cid1:_) clss k1 k2
 = do	let Just cls1	= find (\c -> classKind c == k1) clss
	let Just cls2	= find (\c -> classKind c == k2) clss
		
	addErrors [ErrorUnifyKindMismatch
			{ eKind1	= k1
			, eTypeSource1	= classSource cls1
			, eKind2	= k2
			, eTypeSource2	= classSource cls2 }]
			
	return cid1


-- | Add a forwards to this class.
addClassForwards 
	:: ClassId 		-- class to point to.
	-> [ClassId] 		-- classes to point from.
	-> SquidM ()

addClassForwards cidL_ cids_
 = do	-- sink the cids.
 	cidL		<- sinkClassId cidL_
	cids		<- mapM sinkClassId cids_

	-- add a substitution for each elem of cids.
	graph		<- getsRef stateGraph

	mapM_	(\x -> liftIO $ writeArray 
			(graphClass graph) 
			x (ClassForward x cidL))
		cids
	