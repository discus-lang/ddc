{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Functions to sink classids, by themselves or within types and kinds.
--   These are wrappers around the functions in "DDC.Solve.Graph.Sink" for use in the `SquidM` monad.
module DDC.Solve.State.Sink
	( sinkVar
	, sinkClassId
	, sinkCidsInNode
	, sinkCidsInType
	, sinkCidsInFetter
	, sinkCidsInNodeFst
	, sinkCidsInFetterFst)
where
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.State.Graph
import DDC.Solve.State.Naming
import DDC.Var
import DDC.Type
import DDC.Main.Error
import Data.Array.IO
import Control.Monad.Trans

stage	= "DDC.Solve.Sink"

-- | Convert a var to canonical form
sinkVar :: Var -> SquidM Var
sinkVar	var
 = do	mCid	<- lookupCidOfVar var
	case mCid of
	 Nothing	-> return var
	 Just cid	-> getCanonicalNameOfClass cid
		

-- | Convert the cids in this node type to canonical form.
sinkCidsInNode :: Node -> SquidM Node
sinkCidsInNode nn
 = do	graph		<- getsRef stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInNodeIO classes nn


-- | Convert the cids in this type to canonical form.
sinkCidsInType :: Type -> SquidM Type
sinkCidsInType tt
 = do	graph		<- getsRef stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInTypeIO classes tt


-- | Convert the cids in this type to canonical form.
sinkCidsInFetter :: Fetter -> SquidM Fetter
sinkCidsInFetter ff
 = do	graph		<- getsRef stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInFetterIO classes ff


-- | Convert the cids in the first element of this tuple to canonical form.
--	Good for the classTypeSources field of a class.
sinkCidsInNodeFst :: (Node, a) -> SquidM (Node, a)	
sinkCidsInNodeFst (nn, x)
 = do	nn'	<- sinkCidsInNode nn
	return	$ (nn', x)


-- | Convert the cids in the first element of this tuple to canonical form.
--	Good for the classFetterSource field of a class.
sinkCidsInFetterFst :: (Fetter, a) -> SquidM (Fetter, a)
sinkCidsInFetterFst (ff, x)
 = do	ff'	<- sinkCidsInFetter ff
	return	$ (ff', x)

-- | Convert this cid to canconical form.
{-# INLINE sinkClassId #-}
sinkClassId ::	ClassId -> SquidM ClassId
sinkClassId  cid	
 = do	graph		<- getsRef stateGraph
 	let classes	=  graphClass graph
	sinkClassId' classes cid
	
sinkClassId' classes cid
 = do	mClass	<- liftIO (readArray classes cid)
 	case mClass of
		ClassForward _ cid'	-> sinkClassId' classes cid'
		ClassUnallocated{}	-> panic stage $ "sinkClassId': class is unallocated"
		ClassFetter{}		-> return cid
		ClassFetterDeleted{}	-> return cid
		Class{}			-> return cid
