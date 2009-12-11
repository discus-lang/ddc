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

	, (tt_packed, tsLoop)	
		<- runState 
			(liftM toFetterFormT $ packT1 False Map.empty $ toConstrainFormT tt) 
			[]
	= let result

		-- if we've hit loops in the substitution then bail out early
		| not $ null tsLoop 	= (tt_packed, tsLoop)
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
	, (tt_packed, tsLoop)	
		<- runState 
			(packT1 True Map.empty $ toConstrainFormT tt) 
			[]
	, tt'		<- toFetterFormT $ inlineFsT $ tt_packed
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
	, (tt_packed, tsLoop)	
		<- runState (packT1 True Map.empty $ toConstrainFormT tt) []
 	, tt'	<- crushT $ toFetterFormT $ inlineFsT $ tt_packed
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
	, (tt_packed, tsLoop)	
		<- runState (liftM toFetterFormT $ packT1 True Map.empty $ toConstrainFormT tt) []
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
	TConstrain t1 crs
	 -> do	t1'	<- packT1 ld ls t1
	 	case t1' of
			TForall b k tBody	
			 -> do	tBody'	<- packTFettersLs ld ls $ TConstrain t1' crs
			 	return	$ TForall b k tBody'

			_	-> packTFettersLs ld ls $ TConstrain t1' crs

	-- sums
	TSum k ts
	 -> do	ts'	<- mapM (packT1 True ls) ts
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

			TConstrain (TBot k) _
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
	TConstrain TError{} _
	 -> return tt

 	TConstrain t crs@(Constraints { crsEq, crsMore, crsOther })
	 -> do	let ls'	= Map.union crsEq ls

		tPacked		<- packT1 ld ls' t
		
		crsEq_packed	<- liftM Map.fromList $ mapM (packFetterLsTT ld ls') $ Map.toList crsEq
		crsOther_packed	<- mapM (packFetterLs   ld ls') crsOther
		let crsPacked	= Constraints crsEq_packed crsMore crsOther_packed


		-- inline TBots, and fetters that are only referenced once
		crsInlined	<- inlineCrs crsPacked

		-- delete constraints that aren't reachable from the body of the type
		let crsRestrict	= restrictCrs tPacked crsInlined
			
		-- add the restricted fetters back to the type.
		let tFinal	= addConstraints crsRestrict tPacked

		-- bail out if we find an infinite type problem
		tsLoops		<- get
		if   (crsSize crsRestrict < crsSize crs)
		  && null tsLoops 
	    	 	then packTFettersLs ld ls tFinal
			else return tFinal

	_ -> return tt

crsSize :: Constraints -> Int
crsSize (Constraints { crsEq, crsMore, crsOther})
	= Map.size crsEq + Map.size crsMore + length crsOther


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
	FConstraint v crs
	 -> do	crs'	<- mapM (packT1 True ls) crs
	 	return	$ FConstraint v crs'

	_ -> return ff
	
packFetterLsTT ld ls (t1, t2)
 = do	t2'	<- packT1 ld ls t2
	return	(t1, crushT t2')
 


-- InlineFs ----------------------------------------------------------------------------------------
-- | Inline fetters into themselves.
--
inlineCrs :: Constraints -> SubM Constraints
inlineCrs crs@Constraints { crsEq, crsMore, crsOther }
 = {-# SCC "inlineCrs" #-}
   do
	-- a substitution with all constraints
	let sub	= crsEq

	-- a substitutions with only data constraints
	let subD = Map.filterWithKey
			(\t x -> (let Just k = kindOfType t in resultKind k) == kValue)
			crsEq

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
			FConstraint v ts 
			 -> do	ts'	<- mapM (subTT_cutM sub (Set.empty)) ts

				return $ FConstraint v ts'
		
			_ -> return ff
	
	let subTT (t1, t2)
		= do	t2'	<- subRHS t1 t2
			return	$ (t1, t2')


	-- substitute into all the fetters
	crsEq_sub	<- liftM Map.fromList $ mapM subTT $ Map.toList crsEq
	crsMore_sub	<- liftM Map.fromList $ mapM subTT $ Map.toList crsMore
	crsOther_sub	<- mapM subF crsOther
	
	let crs_sub	= Constraints crsEq_sub crsMore_sub crsOther_sub 
	
	return crs_sub


-- | Inline Effect and Closure fetters, regardless of the number of times they are used
--
inlineFsT :: Type -> Type
inlineFsT tt
 = {-# SCC "inlineFsT" #-}
   case tt of
 	TConstrain t crs@Constraints { crsEq, crsMore, crsOther }
	 -> let	sub	= crsEq
		crsEq'	= Map.mapWithKey 
				(\t1 t2 -> let (t2', []) = subTT sub t2
				           in  t2')
				crsEq
	
		(t', []) = subTT sub t

	    in	addConstraints (Constraints crsEq' crsMore crsOther) t'
	
	_	-> tt	     	

-- RestrictFs --------------------------------------------------------------------------------------
-- | Restrict the list of TLet fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictCrs :: Type -> Constraints -> Constraints
restrictCrs tt crs@Constraints { crsEq, crsMore, crsOther }
 = {-# SCC "restrictCrs" #-}
   let	reachFWheresMap	
		= Map.unions 
			[ Map.map (Set.fromList . collectTClassVars) crsEq
			, Map.map (Set.fromList . collectTClassVars) crsEq ]
 
 	tsSeed		= Set.fromList 
			$ collectTClassVars tt
			++ concat [catMap collectTClassVars ts 
					| FConstraint v ts	<- crsOther ]

	tsReachable	= tsSeed `Set.union` graphReachableS reachFWheresMap tsSeed

	keepEq   t1  t2 | t1 == t2	= False
	keepEq   t1 (TBot _)		= False
	keepEq   t1 _			= Set.member t1 tsReachable
	
	keepMore t1  t2 | t1 == t2	= False
	keepMore t1 (TBot _)		= False
	keepMore _  _			= True
	
	crsEq_restrict		= Map.filterWithKey keepEq   crsEq
	crsMore_restrict	= Map.filterWithKey keepMore crsMore
	
	crs_restrict		= Constraints crsEq_restrict crsMore_restrict crsOther
	
   in	crs_restrict
	
	
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

