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

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Util.Map	as Map
import qualified Data.Set	as Set
import Util
import Util.Graph.Deps

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
 = case liftM resultKind $ takeKindOfType tt of
 	Just KValue	-> packData_noLoops  tt
 	Just KNil	-> packData_noLoops  tt


	Just KEffect	-> packEffect_noLoops tt
	 
	Just KClosure	-> packClosure_noLoops tt
	Just KRegion	-> packRegion  tt

packType :: Type -> (Type, [(Type, Type)])
packType tt
 = case liftM resultKind $ takeKindOfType tt of
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
	| Just k		<- liftM resultKind $ takeKindOfType tt
	, elem k [KValue, KNil]

	, (tt_packed, tsLoop)	<- runState (packTypeLs False Map.empty tt) []
	= let result

		-- if we've hit loops in the substitution then bail out early
		| not $ null tsLoop 	= (tt_packed, tsLoop)
			
		-- type has stopped moving.
		| tt_packed == tt	= (tt_packed, [])

		-- keep packing
		| otherwise		= packData tt_packed
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
	| Just KEffect		<- takeKindOfType tt
	, (tt_packed, tsLoop)	<- runState (packTypeLs True Map.empty tt) []
	, tt'			<- inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt', tsLoop)
		| tt == tt'		= (tt', [])
		| otherwise		= packEffect tt'
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
	| Just KClosure		<- takeKindOfType tt
	, (tt_packed, tsLoop)	<- runState (packTypeLs True Map.empty tt) []
 	, tt'			<- crushT $ inlineFsT $ tt_packed
	= let result
		| not $ null tsLoop	= (tt_packed, tsLoop)
		| tt == tt'		= (tt', [])
		| otherwise		= packClosure tt'
	  in result

	| otherwise
	= panic stage 
	 	$ "packClosure: not a closure\n"
		% "  c = " % tt % "\n\n"

-- | Pack a region
packRegion :: Region -> Region
packRegion tt
	| Just KRegion		<- takeKindOfType tt
	, (tt_packed, tsLoop)	<- runState (packTypeLs True Map.empty tt) []
	, null tsLoop
	= if tt == tt_packed
		then tt_packed
		else packRegion tt_packed


-- PackTypeLs --------------------------------------------------------------------------------------
packTypeLs 
	:: Bool			-- whether to substitute effect and closure constructors as well
	-> Map Type Type	-- types to substitute for
	-> Type			-- type to substitute into
	-> SubM Type		-- result type

packTypeLs ld ls tt
 = {-# SCC "packTypeLs" #-}
   case tt of
	TError{}
 	 -> return tt

	-- push foralls under closure tags
	TForall vks (TFree v1 t)	
	 -> do	t'	<- packTypeLs ld ls t
	 	return	$ TFree v1 (TForall vks t')

 	TForall vks t	
	 -> do	t'	<- packTypeLs ld ls t
	   	return	$ TForall vks t'

	-- keep fetters under foralls.
	TFetters fs (TForall vks t)
	 -> do	t'	<- packTypeLs ld ls t
		return	$ TForall vks (TFetters fs t')

	TFetters fs t
	 -> packTFettersLs ld ls tt

	TSum k@KClosure ts
	 -> do	ts'	<- liftM nub $ mapM (packTypeLs True ls) ts
	    	return	$ makeTSum k (maskMerge ts')

	TSum k@KEffect ts
	 -> do	ts'	<- liftM nub $ mapM (packTypeLs True ls) ts
	    	return	$ makeTSum k ts'

	TMask k1 (TMask k2 t1 t2) t3
	 	| k1 == k2
		-> return $ makeTMask k1 t1 (makeTSum KClosure [t2, t3])

	-- don't substitute into the lhs of mask expression so we end up with
	--	c2 = c3 \ x ; c3 = { x : ... }   etc
	TMask k t1 t2
	 -> do	t1'	<- packTypeLs ld ls t1
	 	t2'	<- packTypeLs True ls t2
	    	return	$ makeTMask k t1' t2'

	-- try and flatten out TApps into their specific constructors
	TApp (TApp (TApp (TApp (TCon TyConFun{}) t1) t2) eff) clo
	 -> 	return	$ TFun t1 t2 eff clo
	    
	TApp (TCon (TyConData { tyConName, tyConKind })) t2
	 ->	return	$ TData tyConKind tyConName [t2]
	
	TApp (TData k v ts) t2
	 ->	return	$ TData k v (ts ++ [t2])
	 
	TApp t1 t2
	 -> do 	t1'	<- packTypeLs ld ls t1
	 	t2'	<- packTypeLs ld ls t2
		return	$ TApp t1' t2'

	TCon{} -> return tt
	    
	TVar k v2
	 -> case Map.lookup tt ls of
	 	-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TVar{}			-> return t'

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> return tt
		Just TEffect{}	| not ld	-> return tt		
		Just TFree{}	| not ld	-> return tt
		Just TMask{}	| not ld	-> return tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> return tt

		Just t'				-> return t'
		Nothing				-> return tt


	TTop k	-> return tt
	TBot k 	-> return tt
		 
	-- data
	TData k v ts
	 -> do	ts'	<- mapM (packTypeLs ld ls) ts
	    	return	$ TData k v ts'

 	TFun t1 t2 eff clo
	 -> do	t1'	<- packTypeLs ld ls t1
	 	t2'	<- packTypeLs ld ls t2
		eff'	<- packTypeLs ld ls eff
		clo'	<- packTypeLs ld ls clo
		return	$ TFun t1' t2' eff' clo'

	-- effect
	TEffect vE [TData k vD (TVar KRegion r : ts)]
	  | vE == primReadH
	  -> return $ TEffect primRead [TVar KRegion r]

	TEffect v ts
	 -> do	ts'	<- mapM (packTypeLs ld ls) ts
		return	$ TEffect v ts'
	    
		    
	-- closure
	TFree v1 (TFree v2 t)			
		-> return $ TFree v1 t

	TFree v1 (TBot KClosure)
		-> return $ TBot KClosure

	TFree v1 (TFetters _ (TBot KClosure))
		-> return $ TBot KClosure

	TFree v1 (TDanger t1 (TFree v t2))
		-> return $ TFree v1 (TDanger t1 t2)

	TFree v t 	
	 -> do	t'	<- packTypeLs True ls t
	 	return	$ TFree v t'

	-- danger
	TDanger t1 (TBot KClosure)
		-> return $ TBot KClosure

	TDanger t1 (TSum KClosure ts)
	 -> do	let ts'	= map (TDanger t1) ts
	 	return	$ makeTSum KClosure ts'

	TDanger t1 t2	
	 -> do	t2'	<- packTypeLs True ls t2
	 	return	$ TDanger t1 t2'
	 
	TTag{}	-> return tt
	    
	-- wildcards
	TWild{}	-> return tt

	-- solver
	TClass k cid1
	 -> case Map.lookup tt ls of
		-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TClass{}		-> return t'

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> return tt
		Just TEffect{}	| not ld	-> return tt		
		Just TFree{}	| not ld	-> return tt
		Just TMask{}	| not ld	-> return tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> return tt

		-- hrm.. don't substitute for Effects and Closures when extracting un-generalised types.
		--	Or we risk loosing port vars in the substitution.
		--	Only Type.Scheme follows this codepath - but we should handle this a different way
		--	perhaps a flag to pack.
		Just t'
			| k == KValue		-> return t'
			| otherwise		-> return tt

		Nothing				-> return tt

	-- sugar
	TElaborate ee t
	 -> do	t'	<- packTypeLs ld ls t
		return	$ TElaborate ee t'

	_ -> panic stage
		$ "packTypeLs: no match for " % show tt
		    

-- Keep repacking a TFetters until the number of Fetters in it stops decreasing 
--	(or we hit an infinite type error)

--	ie, find a fixpoint of restrictFs . packFettersLs
--
packTFettersLs 
	:: Bool
	-> Map Type Type
	-> Type 
	-> SubM Type

packTFettersLs ld ls tt
 = {-# SCC "packTFettersLs" #-}
   case tt of
	TFetters fs (TError{})
	 -> return tt

 	TFetters fs t
	 -> do	let ls'	= Map.union 
	 			(Map.fromList [(t1, t2) | FLet t1 t2 <- fs])
				ls

		tPacked		<- packTypeLs ld ls' t

		fsPacked	<- mapM (packFetterLs ld ls') fs

		-- erase trivial closures
		let (tZapped, fsZapped)
				= mapAccumL (zapCoveredTMaskF ls') tPacked fsPacked
		
		-- inline TBots, and fetters that are only referenced once
		fsInlined	<- inlineFs fsZapped

		let fsSorted	= sortFs		
				$ restrictFs tZapped
				$ fsInlined
				
		let tFinal	= addFetters fsSorted tZapped

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
 	FLet t1 t2
	 -> do	t2'	<- packTypeLs ld ls t2
	 	return	$ FLet t1 (crushT t2')

	FConstraint v ts
	 -> do	ts'	<- mapM (packTypeLs True ls) ts
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
		 	FLet t1 t2	-> Just (t1, t2)
--			FMore t1 t2	-> Just (t1, TSum (kindOfType t1) [t1, t2])
			_		-> Nothing
	
	let sub	= Map.fromList	
			$ catMaybes
			$ map takeSub fs

	-- a substitutions with only data fetters.
	let subD = Map.filterWithKey
			(\t x -> (let Just k = takeKindOfType t in resultKind k) == KValue)
			sub

	-- Don't substitute closures and effects into data, it's too hard to read.				
	--	Also bail out early if one of the substitutions hits an infinite
	--	type error, otherwise we'll get multiple copies of the error
	--	in the SubM state.
	let subRHS t1 t2
		= do	tsLoops	<- get
			if null tsLoops
			 then case liftM resultKind $ takeKindOfType t1 of
				Just KValue	-> subTT_cutM subD (Set.singleton t1) t2
				Just _		-> subTT_cutM sub  (Set.singleton t1) t2

			 else return t2
				
	-- substitute in the the RHSs
	let subF ff
		= case ff of
			FLet  t1 t2 	 
			 -> do	t2'	<- subRHS t1 t2
			 	return	$ FLet t1 t2'
			 
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
 	TFetters fs t
	 -> let	sub	= Map.fromList
			$ catMaybes
			$ map (\f -> case f of
					FLet t1 t2	-> Just (t1, t2)
					_ 		-> Nothing)
			$ fs

		fs'	= map (\f -> case f of
					FLet t1 t2 	
					 -> let (t2', []) = subTT sub t2
					    in  FLet t1 t2'

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
   let	reachFLetsMap
 		= Map.fromList
		$ catMaybes
		$ map (\f -> case f of
			FLet  t1 t2	-> Just (t1, Set.fromList $ collectTClassVars t2)
			FMore t1 t2	-> Just (t1, Set.fromList $ collectTClassVars t2)
			FProj j v t1 t2	-> Nothing
			FConstraint{}	-> Nothing)
		$ ls
 
 	tsSeed		= Set.fromList 
			$ collectTClassVars tt
			++ concat [catMap collectTClassVars ts 
					| FConstraint v ts	<- ls]

	tsReachable	= tsSeed `Set.union` graphReachableS reachFLetsMap tsSeed
	 
   in	filter (\f -> case f of
			FLet t1 t2
			 | t1 == t2	-> False
			 
			FMore t1 t2
			 | t1 == t2	-> False
			
			FMore  t (TBot _) -> False
			 
			FLet t (TBot _)	-> False
   			FLet t _	-> Set.member t tsReachable
			_		-> True)
		$ ls



-- Sort Fetters ------------------------------------------------------------------------------------
-- | Sort fetters so effect and closure information comes out first in the list.
sortFs :: [Fetter] -> [Fetter]
sortFs fs
 = let	isLetK k f
  	 = case f of
	 	FLet _ t2	-> (let Just k1 = takeKindOfType t2 in k1) == k
		FMore _ t2	-> (let Just k1 = takeKindOfType t2 in k1) == k
		_		-> False

	([ fsData,   fsEffect, fsClosure], fsRest)	
		= partitionFs 
			[ isLetK KValue, 	isLetK KEffect, 	isLetK KClosure]
			fs
				
    in	   fsData 	++ fsEffect ++ fsClosure ++ fsRest
    
sortFsT :: Type -> Type
sortFsT tt
 = case tt of
 	TFetters fs t	-> TFetters (sortFs fs) t
	_		-> tt


-- Zap Covered -------------------------------------------------------------------------------------
-- | Erase TMasks where the LHS can only ever contain the values present in the RHS
-- eg
--	  1 -(4)> 2 -(5)> 3
--        :- 4        = 5 \\ f
--        ,  5        = f :: ...
--
--	5 is not substituted by inlineFs1 because it is referenced twice.
--	However, the binding for 4 is redundant because 5 can only ever contain f.
--	
-- rewrite to:
--	  1 -> 2 -(5)> 3
--        :-  5        = f :: ...
--
zapCoveredTMaskF 
	:: Map Type Type 
	-> Type 
	-> Fetter 
	-> (Type, Fetter)

zapCoveredTMaskF ls tt ff
 = zapCoveredTMaskF' ls tt ff
	
zapCoveredTMaskF' ls tt ff
	| FLet t1 (TMask k t2 t3)	<- ff
	, Just t2l			<- Map.lookup t2 ls
	, coversCC t2l t3
	, (tt', [])			<- subTT (Map.singleton t1 (TBot k)) tt
	= ( tt'
	  , FLet t1 (TBot k))

	| otherwise
	= (tt, ff)
		 
coversCC (TFree v1 _) 	(TTag v2)	= v1 == v2
coversCC _		_		= False



-- MaskMerge ---------------------------------------------------------------------------------------
-- Merges a list of mask expressions.
--	We need to do this to avoid exponential blowup of closure expressions
--	as per test/Typing/Loop/Loop1
--	
--	eg:	$c1 \ x, $c1 \ y, $c1 \ z	-> $c1 \ [x, y, z]
--
maskMerge :: [Closure] -> [Closure]
maskMerge cc
 = case maskMerge_run False [] cc of
 	Nothing		-> cc
	Just cc'	-> maskMerge cc'

-- we're at the end of the list, but we merged something along the way
maskMerge_run True acc []
	= Just (reverse acc)
	
-- we're at the end of the list, and nothing merged
maskMerge_run False acc []
	= Nothing

maskMerge_run hit acc (x:xs)
	-- try and merge this element into the rest of the list
	| Just xs'	<- maskMerge_step x xs
	= maskMerge_run True acc xs'
	
	| otherwise
	= maskMerge_run hit (x : acc) xs

maskMerge_step z (x:xs)
	-- we could merge with the current element
	| Just x'	<- maskMerge1 z x
	= Just (x' : xs)
	
	-- try and merge with later elementts
	| Just xs'	<- maskMerge_step z xs
	= Just (x : xs')

	| otherwise
	= Nothing
	
maskMerge_step z []	
	= Nothing
	
maskMerge1 cc1 cc2
	| TMask k1 c1 t1	<- cc1
	, TMask k2 c2 t2	<- cc2
	, k1 == k2
	, c1 == c2
	= Just $ makeTMask k1 c1 (makeTSum k1 [t1, t2])

	| otherwise
	= Nothing
