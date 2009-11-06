module Type.Util.Pack
	( packType, packType_noLoops
	, packData, packData_noLoops
	, packEffect, packEffect_noLoops
	, packClosure, packClosure_noLoops
	, sortFsT)

where

import Type.Exp
import Type.Pretty
import Type.Plate.Collect
import Type.Util.Bits
import Type.Util.Substitute
import Type.Util.Kind

import Shared.VarPrim
import Shared.Pretty
import Shared.Error

import Util
import Util.Graph.Deps
import Control.Monad.State
import qualified Util.Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace

-----
stage	= "Type.Util.Pack"

{-
debug	= True
trace ss x
	= if debug 
		then (Debug.Trace.trace (pprStrPlain ss) x)
		else x
-}

------------------------
-- packType
--	Pack a type into the standard form.	
--
--	* Do a full substition into the data portion of types.
--	* Erase fetters which aren't reachable from the type shape.
--	* Substitute TPure/TEmpty and crush TSums
--	* Erase TFrees if they contain no classids.
--
packType_noLoops :: Type -> Type
packType_noLoops tt
 = case liftM resultKind $ kindOfType tt of
 	Just KValue	-> packData_noLoops  tt
 	Just KNil	-> packData_noLoops  tt


	Just KEffect	-> packEffect_noLoops tt
	 
	Just KClosure	-> packClosure_noLoops tt
	Just KRegion	-> packRegion  tt

packType :: Type -> (Type, [(Type, Type)])
packType tt
 = case liftM resultKind $ kindOfType tt of
 	Just KValue	-> packData   tt
	Just KEffect	-> packEffect tt
	 
	Just KClosure	-> packClosure tt
	Just KRegion	-> (packRegion  tt, [])


-- PackData ----------------------------------------------------------------------------------------
-- | Pack a value type, and panic if we find loops in the substitution.
packData_noLoops  :: Data -> Data
packData_noLoops tt
 = case packData tt of
 	(tt_packed, [])		-> tt_packed
	(tt_packed, tsLoops)
	 	-> panic stage 
		$ "packData: got loops\n"
		% "loops:\n" %> punc "\n" tsLoops % "\n"

-- | Pack a value type
packData :: Data -> (Data, [(Type, Type)])
packData tt
	| Just k		<- liftM resultKind $ kindOfType tt
	, elem k [KValue, KNil]

	, (tt_packed, tsLoop)	<- runState (packT1 False Map.empty tt) []
	= let result

		-- if we've hit loops in the substitution then bail out early
		| not $ null tsLoop 	= (tt_packed, tsLoop)
			
		-- type has stopped moving.
--		| tt_packed == tt	= (tt_packed, [])

		-- keep packing
--		| otherwise		= packData tt_packed

		| otherwise		= (tt_packed, [])
	  in  result



-- | Pack an effect into the standard form
--	Regular packType won't flatten out recursive effects like this example:
--
--	!{!3386; Base.!ReadH (Data.Bool.Bool %3368)} 
--		:- !3386      = !{!3369; Base.!ReadT (Base.Int %481)}
--
packEffect_noLoops :: Effect -> Effect
packEffect_noLoops tt
 = case packEffect tt of
 	(tt_packed, [])		-> tt_packed
	(tt_packed, tsLoops)
		-> panic stage
		$  "packEffects: got loops\n"


packEffect :: Effect -> (Effect, [(Type, Type)])
packEffect tt
	| Just KEffect		<- kindOfType tt
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
	, tt'			<- inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt', tsLoop)
--		| tt == tt'		= (tt', [])
--		| otherwise		= packEffect tt'
		| otherwise		= (tt', [])
	  in result


-- 
packClosure_noLoops :: Closure -> Closure
packClosure_noLoops tt
 = case packClosure tt of
 	(tt_packed, [])		-> tt_packed
	(tt_packed, tsLoops)
		-> panic stage
		$ "packClosure: got loops\n"
		
packClosure tt
	| Just KClosure		<- kindOfType tt
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
 	, tt'			<- crushT $ inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt_packed, tsLoop)
--		| tt == tt'		= (tt', [])
--		| otherwise		= packClosure tt'
		| otherwise		= (tt', [])
	  in result

	| otherwise
	= panic stage 
	 	$ "packClosure: not a closure\n"
		% "  c = " % tt % "\n\n"

-- | Pack a region
packRegion :: Region -> Region
packRegion tt
	| Just KRegion		<- kindOfType tt
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
	, null tsLoop
	= tt_packed
	
--	if tt == tt_packed
--		then tt_packed
--		else packRegion tt_packed


-- packT1 --------------------------------------------------------------------------------------
packT1 
	:: Bool			-- whether to substitute effect and closure constructors as well
	-> Map Type Type	-- types to substitute for
	-> Type			-- type to substitute into
	-> SubM Type		-- result type

packT1 ld ls tt
 = {-# SCC "packT1" #-}
   case tt of
	TError{}
 	 -> return tt

	-- push foralls under closure tags
	TForall v1 k1 tBody
	 -> do	tBody'	<- packT1 ld ls tBody
		case tBody' of
		 	TFree v2 t2		-> return $ TFree v2 (TForall v1 k1 t2)
			tBody'			-> return $ TForall v1 k1 tBody'

	-- keep fetters under foralls.
	TFetters t1 fs
	 -> do	t1'	<- packT1 ld ls t1
	 	case t1' of
			TForall b k tBody	
			 -> do	tBody'	<- packTFettersLs ld ls $ TFetters t1' fs
			 	return	$ TForall b k tBody'

			_	-> packTFettersLs ld ls $ TFetters t1' fs

	-- sums
	TSum k ts
	 -> do	ts'	<- liftM nub $ mapM (packT1 True ls) ts
		return	$ makeTSum k ts'
	
			
	-- try and flatten out TApps into their specific constructors
	TApp t1 t2
	 -> do	t1'	<- packT1 ld ls t1
	 	t2'	<- packT1 ld ls t2
		
		case TApp t1' t2' of
			TApp (TApp (TApp (TApp (TCon TyConFun{}) t1) t2) eff) clo
	 		 -> return	$ TFun t1 t2 eff clo
	    
			TApp (TCon (TyConData { tyConName, tyConDataKind })) t2
			 -> return	$ TData tyConDataKind tyConName [t2]
	
			TApp (TData k v ts) t2
			 -> return	$ TData k v (ts ++ [t2])
			
			tt'	-> return tt'
			
	TCon{} -> return tt
	    
	TVar k v2
	 -> case Map.lookup tt ls of
	 	-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TVar{}			-> return t' -- packT1 ld ls t'.. detect loops

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> return tt
		Just TEffect{}	| not ld	-> return tt		
		Just TFree{}	| not ld	-> return tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> return tt

		Just t'				-> return t' -- packT1 ld ls t'.. detect loops
		Nothing				-> return tt


	TTop k	-> return tt
	TBot k 	-> return tt
		 
	-- data
	TData k v ts
	 -> do	ts'	<- mapM (packT1 ld ls) ts
	    	return	$ TData k v ts'

 	TFun t1 t2 eff clo
	 -> do	t1'	<- packT1 ld ls t1
	 	t2'	<- packT1 ld ls t2
		eff'	<- packT1 ld ls eff
		clo'	<- packT1 ld ls clo
		return	$ TFun t1' t2' eff' clo'

	-- effect
	TEffect vE ts
	 -> do	ts'	<- mapM (packT1 ld ls) ts
	 	
		
		let result
			| vE == primReadH
			, [TData k vD (TVar KRegion r : _)]	<- ts'
			= TEffect primRead [TVar KRegion r]
			
			| otherwise
			= TEffect vE ts'

		return result
			
	-- closure
	TFree v1 t2
	 -> do	t2'	<- packT1 ld ls t2
	 	case t2' of
			TFree v2 t2'			-> return $ TFree v1 t2'
			TBot KClosure			-> return $ TBot KClosure
			TFetters (TBot KClosure) _	-> return $ TBot KClosure
			TDanger t21 (TFree v t22)	-> return $ TFree v1 (TDanger t21 t22)
			_				-> return $ TFree v1 t2'						


	-- danger
	TDanger t1 t2
	 -> do	t2'	<- packT1 ld ls t2
	 	case t2' of
			TBot KClosure		-> return $ TBot KClosure
			TSum KClosure t2s	-> return $ makeTSum KClosure (map (TDanger t1) t2s)
			_			-> return $ TDanger t1 t2'

	-- solver
	TClass k cid1
	 -> case Map.lookup tt ls of
		-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TClass{}		-> return t' -- packT1 ld ls t'.. detect loops

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> return tt
		Just TEffect{}	| not ld	-> return tt		
		Just TFree{}	| not ld	-> return tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> return tt

		-- hrm.. don't substitute for Effects and Closures when extracting un-generalised types.
		--	Or we risk loosing port vars in the substitution.
		--	Only Type.Scheme follows this codepath - but we should handle this a different way
		--	perhaps a flag to pack.
		Just t'
			| k == KValue		-> return t' -- packT1 ld ls t'.. detect loops
			| otherwise		-> return tt

		Nothing				-> return tt

	-- sugar
	TElaborate ee t
	 -> do	t'	<- packT1 ld ls t
		return	$ TElaborate ee t'

	_ -> panic stage
		$ "packT1: no match for " % show tt
		    

-- Keep repacking a TFetters until the number of Fetters in it stops decreasing 
--	(or we hit an infinite type error)

--	ie, find a fixpoint of restrictFs . packFettersLs
--
packTFettersLs 
	:: Bool
	-> Map Type Type
	-> Type			-- should be pre-packed
	-> SubM Type

packTFettersLs ld ls tt
 = {-# SCC "packTFettersLs" #-}
   case tt of
	TFetters TError{} fs
	 -> return tt

 	TFetters t fs
	 -> do	let ls'	= Map.union 
	 			(Map.fromList [(t1, t2) | FWhere t1 t2 <- fs])
				ls

		tPacked		<- packT1 ld ls' t
		fsPacked	<- mapM (packFetterLs ld ls') fs

		-- inline TBots, and fetters that are only referenced once
		fsInlined	<- inlineFs fsPacked

		let fsSorted	= sortFs		
				$ restrictFs tPacked
				$ fsInlined
				
		let tFinal	= addFetters fsSorted tPacked

		-- bail out if we find an infinite type problem
		tsLoops		<- get
		if   (length fsSorted < length fs)
		  && null tsLoops 
	    	 	then packTFettersLs ld ls tFinal
			else return tFinal

	_ -> return tt


-- | Pack the type in the RHS of a TLet
--
packFetterLs 
	:: Bool 
	-> Map Type Type 
	-> Fetter 
	-> SubM Fetter

packFetterLs ld ls ff
 = {-# SCC "packFetterLs" #-}
   case ff of
 	FWhere t1 t2
	 -> do	t2'	<- packT1 ld ls t2
	 	return	$ FWhere t1 (crushT t2')

	FConstraint v ts
	 -> do	ts'	<- mapM (packT1 True ls) ts
	 	return	$ FConstraint v ts'

	_ -> return ff


-- InlineFs ----------------------------------------------------------------------------------------
-- | Inline fetters into themselves.
--
inlineFs :: [Fetter] -> SubM [Fetter]
inlineFs fs
 = {-# SCC "inlineFs1" #-}
   do
   	-- build a substitution from the fetters
	let takeSub ff
		 = case ff of
		 	FWhere t1 t2	-> Just (t1, t2)
--			FMore t1 t2	-> Just (t1, TSum (kindOfType t1) [t1, t2])
			_		-> Nothing
	
	let sub	= Map.fromList	
			$ catMaybes
			$ map takeSub fs

	-- a substitutions with only data fetters.
	let subD = Map.filterWithKey
			(\t x -> (let Just k = kindOfType t in resultKind k) == KValue)
			sub

	-- Don't substitute closures and effects into data, it's too hard to read.				
	--	Also bail out early if one of the substitutions hits an infinite
	--	type error, otherwise we'll get multiple copies of the error
	--	in the SubM state.
	let subRHS t1 t2
		= do	tsLoops	<- get
			if null tsLoops
			 then case liftM resultKind $ kindOfType t1 of
				Just KValue	-> subTT_cutM subD (Set.singleton t1) t2
				Just _		-> subTT_cutM sub  (Set.singleton t1) t2

			 else return t2
				
	-- substitute in the the RHSs
	let subF ff
		= case ff of
			FWhere  t1 t2 	 
			 -> do	t2'	<- subRHS t1 t2
			 	return	$ FWhere t1 t2'
			 
			FMore t1 t2	 
			 -> do	t2'	<- subRHS t1 t2
			    	return $ FMore t1 t2'

			FConstraint v ts 
			 -> do	ts'	<- mapM (subTT_cutM sub (Set.empty)) ts

				return $ FConstraint v ts'
		
			_ -> return ff

	-- substitute into all the fetters
	fs'	<- mapM subF fs

	return fs'


-- | Inline Effect and Closure fetters, regardless of the number of times they are used
--
inlineFsT :: Type -> Type
inlineFsT tt
 = {-# SCC "inlineFsT" #-}
   case tt of
 	TFetters t fs
	 -> let	sub	= Map.fromList
			$ catMaybes
			$ map (\f -> case f of
					FWhere t1 t2	-> Just (t1, t2)
					_ 		-> Nothing)
			$ fs

		fs'	= map (\f -> case f of
					FWhere t1 t2 	
					 -> let (t2', []) = subTT sub t2
					    in  FWhere t1 t2'

					_		-> f)
			$ fs

		(t', []) = subTT sub t

	    in	addFetters fs' t'

	_	-> tt	     	

-- RestrictFs --------------------------------------------------------------------------------------
-- | Restrict the list of TLet fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictFs :: Type -> [Fetter] -> [Fetter]
restrictFs tt ls
 = {-# SCC "restrictFs" #-}
   let	reachFWheresMap
 		= Map.fromList
		$ catMaybes
		$ map (\f -> case f of
			FWhere  t1 t2	-> Just (t1, Set.fromList $ collectTClassVars t2)
			FMore t1 t2	-> Just (t1, Set.fromList $ collectTClassVars t2)
			FProj j v t1 t2	-> Nothing
			FConstraint{}	-> Nothing)
		$ ls
 
 	tsSeed		= Set.fromList 
			$ collectTClassVars tt
			++ concat [catMap collectTClassVars ts 
					| FConstraint v ts	<- ls]

	tsReachable	= tsSeed `Set.union` graphReachableS reachFWheresMap tsSeed
	 
   in	filter (\f -> case f of
			FWhere t1 t2
			 | t1 == t2	-> False
			 
			FMore t1 t2
			 | t1 == t2	-> False
			
			FMore  t (TBot _) -> False
			 
			FWhere t (TBot _)	-> False
   			FWhere t _	-> Set.member t tsReachable
			_		-> True)
		$ ls



-- Sort Fetters ------------------------------------------------------------------------------------
-- | Sort fetters so effect and closure information comes out first in the list.
sortFs :: [Fetter] -> [Fetter]
sortFs fs
 = let	isLetK k f
  	 = case f of
	 	FWhere _ t2	-> (let Just k1 = kindOfType t2 in k1) == k
		FMore _ t2	-> (let Just k1 = kindOfType t2 in k1) == k
		_		-> False

	([ fsData,   fsEffect, fsClosure], fsRest)	
		= partitionFs 
			[ isLetK KValue, 	isLetK KEffect, 	isLetK KClosure]
			fs
				
    in	   fsData 	++ fsEffect ++ fsClosure ++ fsRest
    
sortFsT :: Type -> Type
sortFsT tt
 = case tt of
 	TFetters t fs	-> TFetters t (sortFs fs) 
	_		-> tt


