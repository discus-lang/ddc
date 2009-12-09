module Type.Util.Pack
	( packType, packType_noLoops
	, packData, packData_noLoops
	, packEffect, packEffect_noLoops
	, packClosure, packClosure_noLoops)
--	, sortFsT)

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

-- | Pack a type into normal form.
--	* Do a full substition into the data portion of types.
--	* Erase fetters which aren't reachable from the type shape.
--	* Substitute TPure/TEmpty and crush TSums
--	* Erase TFrees if they contain no classids.
--
--   The "noloops" version panics if the constraints are recursive through the 
--	value type portion.
--
packType_noLoops :: Type -> Type
packType_noLoops tt
	| k == kValue	= packData_noLoops tt
	| k == kEffect	= packEffect_noLoops tt
	| k == kClosure	= packClosure_noLoops tt
	| k == kRegion	= packRegion tt
	where Just k	= liftM resultKind $ kindOfType tt	

packType :: Type -> (Type, [(Type, Type)])
packType tt
	| k == kValue	= packData tt
	| k == kEffect	= packEffect tt
	| k == kClosure	= packClosure tt
	| k == kRegion	= (packRegion  tt, [])
	where Just k	= liftM resultKind $ kindOfType tt


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
	, elem k [kValue, KNil]

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
	| kindOfType tt	== Just kEffect
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
	, tt'			<- inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt', tsLoop)
		| otherwise		= (tt', [])
	  in result


-- | Pack a closure into the standard form
packClosure_noLoops :: Closure -> Closure
packClosure_noLoops tt
 = case packClosure tt of
 	(tt_packed, [])		-> tt_packed
	(tt_packed, tsLoops)
		-> panic stage
		$ "packClosure: got loops\n"
		
packClosure tt
	| kindOfType tt == Just kClosure
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
 	, tt'			<- crushT $ inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt_packed, tsLoop)
		| otherwise		= (tt', [])
	  in result

	| otherwise
	= panic stage 
	 	$ "packClosure: not a closure\n"
		% "  c = " % tt % "\n\n"


-- | Pack a region into the standard form.
packRegion :: Region -> Region
packRegion tt
	| kindOfType tt	== Just kRegion
	, (tt_packed, tsLoop)	<- runState (packT1 True Map.empty tt) []
	, null tsLoop
	= tt_packed
	

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
		return	$ TApp t1' t2'
					
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
		 
	-- effect
	TEffect vE ts
	 -> do	ts'	<- mapM (packT1 ld ls) ts
	 	
		
		let result
			| vE == primReadH
			, [t']				<- ts
			, Just (vD, k, (TVar kR r : _))	<- takeTData t'
			, kR	== kRegion
			= TEffect primRead [TVar kRegion r]
			
			| otherwise
			= TEffect vE ts'

		return result
			
	-- closure
	TFree v1 t2
	 -> do	t2'	<- packT1 ld ls t2
	 	case t2' of
			TFree v2 t2'			-> return $ TFree v1 t2'
			TBot k 
				| k == kClosure		-> return $ TBot kClosure

			TFetters (TBot k) _
				| k == kClosure		-> return $ TBot kClosure	

			TDanger t21 (TFree v t22)	-> return $ TFree v1 (TDanger t21 t22)
			_				-> return $ TFree v1 t2'						


	-- danger
	TDanger t1 t2
	 -> do	t2'	<- packT1 ld ls t2
	 	case t2' of
			TBot k
				| k == kClosure	
				-> return $ TBot kClosure

			TSum k t2s	
				| k == kClosure	
				-> return $ makeTSum kClosure (map (TDanger t1) t2s)

			_	-> return $ TDanger t1 t2'

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
			| resultKind k == kValue	-> return t' -- packT1 ld ls t'.. detect loops
			| otherwise			-> return tt

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

		let fsSorted	= restrictFs tPacked
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
			(\t x -> (let Just k = kindOfType t in resultKind k) == kValue)
			sub

	-- Don't substitute closures and effects into data, it's too hard to read.				
	--	Also bail out early if one of the substitutions hits an infinite
	--	type error, otherwise we'll get multiple copies of the error
	--	in the SubM state.
	let subRHS t1 t2
		= do	tsLoops	<- get
			if null tsLoops
			 then if (liftM resultKind $ kindOfType t1) == Just kValue
				then subTT_cutM subD (Set.singleton t1) t2
				else subTT_cutM sub  (Set.singleton t1) t2
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


{-
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
			[ isLetK kValue, isLetK kEffect, isLetK kClosure]
			fs
				
    in	   fsData 	++ fsEffect ++ fsClosure ++ fsRest
    
sortFsT :: Type -> Type
sortFsT tt
 = case tt of
 	TFetters t fs	-> TFetters t (sortFs fs) 
	_		-> tt
-}

