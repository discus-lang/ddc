
-- All regions are assumed constant until proven mutable.
--	This means that we can generalise type variables in scheme but find
--	out they were actually are dangerous and non-generalisable later on.
--
-- If we just instantiate bad type vars more than once they could potentially
--	become different types, which makes the system unsound.
--
-- It would be better to do some back-tracking to recover from this, but instead
--	we just check for wrongly generalised type vars each time we do an
--	instantiation and report an error asking for a type sig if there are problems.
--
-- Type sigs are added to the graph before anything else, so we'll see the mutability
--	constraints during generaliation on the next time around.
-- 
module Type.Check.SchemeDanger
	( checkSchemeDangerCid
	, checkSchemeDanger)

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim

import Type.Diagnose
import Type.Exp
import Type.Error
import Type.Util
import Type.Plate.Collect
import Type.State
import Type.Class

import Shared.Error

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
-- stage	= "Type.Check.ReGeneralise"
debug	= True
trace s	= when debug $ traceM s

 
checkSchemeDangerCid :: ClassId -> SquidM [Error]
checkSchemeDangerCid cid
 = do	Just c	<- lookupClass cid
 	checkSchemeDanger c
 
checkSchemeDanger :: Class -> SquidM [Error]
checkSchemeDanger c
	| Class { classKind	= KData 
		, classId	= cid
		, classType	= Just t }	<- c
	, TForall vks x				<- t

	= do	trace 	$ "*   checkReGeneralise\n"
			% "    t = " % t % "\n"

		let clo	= trimClosureC Set.empty Set.empty $ TFree (Var.new "foo") x
		
		let ds	= catMaybes
			$ map (\d -> case d of
					TFree _ (TDanger t1 t2)	-> Just (t1, t2)
					_			-> Nothing)
			$ flattenTSum clo
		
		trace	$ "    clo  = " % clo 	% "\n"
			% "    ds   = " % ds	% "\n"
			
		errs	<- liftM catMaybes
			$ mapM (checkDanger c) ds
			
		trace	$ "    errs  = " % errs	% "\n\n"
			
		return errs

	| otherwise
	= return []
 	
-- Produce an error if the region is mutable
checkDanger :: Class -> (Type, Type) -> SquidM (Maybe Error)
checkDanger (Class 
		{ classId 	= cidScheme
		, classType 	= Just t })
		(tRegion@(TClass KRegion cidR), TVar k varT)

 = do 	mIsMutable	<- regionIsMutable cidR
	
	case mIsMutable of
	 Nothing	-> return Nothing
	 Just fMutable
	  -> do
		varScheme	<- makeClassName cidScheme
		Just srcMutable	<- traceFetterSource primMutable cidR
		return	
			$ Just
			$ ErrorLateConstraint
				{ eScheme 		= (varScheme, t)
				, eMutableRegion	= tRegion
				, eMutableFetter	= fMutable
				, eMutableSrc		= srcMutable
				, eDangerVar		= varT }
			
	
regionIsMutable :: ClassId -> SquidM (Maybe Fetter)
regionIsMutable cid
 = do	Just (Class { classFetters = fs })	
 		<- lookupClass cid

	let fsMutable
		= [ f 	| TFetter f@(FConstraint v _) <- fs
			, v == primMutable ]
		
	fsMutable'	<- mapM updateVC fsMutable
		
	case fsMutable' of
		[]	-> return $ Nothing
		(f:_)	-> return $ Just f
		

-- checkUpdateSoundness
--	Check for update soudness problems in an already generalised scheme.
--
--
{-
checkUpdateSoundness
	:: Var -> Type -> SquidM ()
	
checkUpdateSoundness varT t
 = do	
	let ?fsMutable
		= nub
		$ [f 	| f@(FClass v _) <- collectFetters t
			, Var.bind v == Var.FMutable]

	let dangerTs
		= nub
		$ [t	| t@(TVar v)	<- dangerT [] t]
 
 	trace	$ "*   CheckUpdate.checkUpdate " 	% t 	% "\n"
		% "    fsMutable = " % ?fsMutable		% "\n"
		% "    dangerTs  = " % dangerTs			% "\n"
		% "\n"
 	
	when (not $ isNil dangerTs)
	 $ do
	 	addErrors 
			[ErrorUpdateSoundness 
				{ eVar		= varT
				, eType		= t
				, eTypeDanger	= dangerTs }]
		
	return	()
 
-----
-}
