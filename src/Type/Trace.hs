
module Type.Trace 
	( traceType 
	, traceCidsDown)
where

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util
import Type.Exp
import Type.Base
import Type.State
import Type.Class
import Type.Plate.Collect
import Type.Plate.Trans


-----
-- traceType
--	Extract a type from the graph by tracing down from this cid
--
traceType :: ClassId -> SquidM Type
traceType cid
 = do 	
 	-- See which classes are reachable by tracing down from this one.
 	cidsDown	<- traceCidsDown cid

	-- Build a new let for each of the classes
	fsLet		<- liftM catMaybes
			$  mapM	loadType
			$  Set.toList cidsDown

	k		<- kindOfCid cid
	let tTrace	= TFetters fsLet (TClass k cid)
	return tTrace

loadType cid
 = do	Just c		<- lookupClass cid
	let t		= classType c
	
	case t of
	 TBot k		
	  ->	 return $ Nothing

	 _ -> do	
		t'	<- refreshCids t
		k	<- kindOfCid cid
		return $ Just (FLet (TClass k cid) t')

refreshCids xx
 	= transZM 
		transTableId 
 		{ transCid = \cid -> do
				cid'	<- sinkClassId cid
				return	$ cid' }
		xx 

-----
-- traceCidsDown 
--	Trace out the cids reachable by tracing down from this cid.
--
traceCidsDown :: ClassId -> SquidM (Set ClassId)
traceCidsDown cid
 = traceCidsDowns (Set.singleton cid) Set.empty


traceCidsDowns :: Set ClassId -> Set ClassId -> SquidM (Set ClassId)
traceCidsDowns toVisit visited
 = case takeHead $ Set.toList toVisit of
 	Nothing	-> return visited
	
	Just cid
	 -> do	Just c		<- lookupClass cid
		let visited'	=  Set.insert cid visited

		moreR		<- refreshCids $ collectClassIds $ classType c
		let more	=  Set.fromList moreR

		let toVisit'	= (toVisit `Set.union` more) `Set.difference` visited'
		traceCidsDowns toVisit' visited'


