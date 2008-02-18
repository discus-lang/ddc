module Type.Util.Pack
	( packType
	, packData
	, packEffect
	, packClosure
	, sortFsT)

where

import Type.Exp
import Type.Pretty
import Type.Plate.Collect
import Type.Util.Bits
import Type.Util.Substitute

import Shared.VarPrim
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
debug	= False
trace ss x
	= if debug 
		then (Debug.Trace.trace (pprStr ss) x)
		else x

------------------------
-- packType
--	Pack a type into the standard form.	
--
--	* Do a full substition into the data portion of types.
--	* Erase fetters which aren't reachable from the type shape.
--	* Substitute TPure/TEmpty and crush TSums
--	* Erase TFrees if they contain no classids.
--
packType :: Type -> Type
packType tt
 = case kindOfType tt of
 	KData		-> packData    tt
	KEffect		-> eff' where Just eff' = packEffect tt
	 
	KClosure	-> packClosure tt
	KRegion		-> packRegion  tt


packData  :: Data -> Data
packData tt
-- = trace ("packData\n" %> prettyTS tt % "\n") $
 = packData2 tt
 
packData2 tt
 | KData	<- kindOfType tt
 = let	tt'	= packTypeLs False Map.empty tt
   in	if tt == tt'
   	 then tt'
	 else packData tt'

packRegion :: Region -> Region
packRegion tt
 | KRegion	<- kindOfType tt
 = let 	tt'	= packTypeLs True Map.empty tt
   in	if tt == tt'
   	 then tt'
	 else packRegion tt'

-- | Pack an effect into the standard form
--	Regular packType won't flatten out recursive effects like this example:
--
--	!{!3386; Base.!ReadH (Data.Bool.Bool %3368)} 
--		:- !3386      = !{!3369; Base.!ReadT (Base.Int %481)}
--

packEffect :: Effect -> Maybe Effect
packEffect tt
 | KEffect	<- kindOfType tt
 = let 	tt'	= inlineFsT $ packTypeLs True Map.empty tt
   in	if tt	== tt'
      	 then Just tt'
	 else packEffect tt'

 | otherwise
 = freakout stage
 	("packEffect: not an effect\n"
	% "  e = " % tt % "\n")
	Nothing
	

packClosure :: Closure -> Closure
packClosure tt
 = trace ("packClosure\n" %> prettyTS tt % "\n") $
   packClosure' tt

packClosure' tt
 | KClosure	<- kindOfType tt
 = let 	tt'	= crushT $ inlineFsT $ packTypeLs True Map.empty tt
   in	if tt	== tt'
      	 then tt'
	 else packClosure tt'

 | otherwise
 = panic stage 
 	$ "packClosure: not a closure\n"
	% "  c = " % tt % "\n\n"

-----
packTypeLs 
	:: Bool			-- whether to effect and closure constructors as well
	-> Map Type Type	-- types to substitute for
	-> Type			-- type to substitute into
	-> Type			-- result type

packTypeLs ld ls tt
 = {-# SCC "packTypeLs" #-}
   case tt of
	TError{}
 	 -> tt

	-- push foralls under closure tags
	TForall vks (TFree v1 t)	
	 -> TFree v1 (TForall vks (packTypeLs ld ls t))

 	TForall vks t	
	 -> let	t'	= packTypeLs ld ls t
	    in	TForall vks t'

	-- keep fetters under foralls.
	TFetters fs (TForall vks t)
	 -> TForall vks (TFetters fs (packTypeLs ld ls t))

	TFetters fs t
	 -> packTFettersLs ld ls tt

	TSum k@KClosure ts
	 -> let	ts'	= nub	$ map (packTypeLs True ls) ts
	    in	makeTSum k (maskMerge ts')

	TSum k@KEffect ts
	 -> let	ts'	= nub	$ map (packTypeLs True ls) ts
	    in	makeTSum k ts'

	TMask k1 (TMask k2 t1 t2) t3
	 | k1 == k2
	 -> makeTMask k1 t1 (makeTSum KClosure [t2, t3])

	-- don't substitute into the lhs of mask expression so we end up with
	--	c2 = c3 \ x ; c3 = { x : ... }   etc
	TMask k t1 t2
	 -> let	t1'	= packTypeLs ld ls t1
	 	t2'	= packTypeLs True ls t2
	    
	    in  makeTMask k t1' t2'
	    
	TVar k v2
	 -> case Map.lookup tt ls of
	 	-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TVar{}		-> t'

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> tt
		Just TEffect{}	| not ld	-> tt		
		Just TFree{}	| not ld	-> tt
		Just TMask{}	| not ld	-> tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> tt

		Just t'				-> t'
		Nothing				-> tt


	TTop k	-> tt
	TBot k 	-> tt
		 
	-- data
	TData v ts
	 -> let	ts'	= map (packTypeLs ld ls) ts
	    in	TData v ts'

 	TFun t1 t2 eff clo
	 -> let	t1'	= packTypeLs ld ls t1
	 	t2'	= packTypeLs ld ls t2
		eff'	= packTypeLs ld ls eff
		clo'	= packTypeLs ld ls clo
	    in	TFun t1' t2' eff' clo'

	-- effect
	TEffect v ts
	 -> let	ts'	= map (packTypeLs ld ls) ts
	    in	TEffect v ts'
	    
	-- closure
	TFree v1 (TFree v2 t)			-> TFree v1 t
	TFree v1 (TBot KClosure)		-> TBot KClosure
	TFree v1 (TFetters _ (TBot KClosure))	-> TBot KClosure

	TFree v1 (TDanger t1 (TFree v t2))
		-> TFree v1 (TDanger t1 t2)

	TFree v t 	
		-> TFree v (packTypeLs True ls t)

	-- danger
	TDanger t1 (TBot KClosure)
		-> TBot KClosure

	TDanger t1 (TSum KClosure ts)
		-> makeTSum KClosure 
		$  map (TDanger t1) ts

	TDanger t1 t2	-> TDanger t1 (packTypeLs True ls t2)
	 
	TTag v	-> tt
	    
	-- wildcards
	TWild{}	-> tt

	-- solver
	TClass k cid1
	 -> case Map.lookup tt ls of
		-- always substitute in trivial @cid1 = @cid2 constraints.
		Just t'@TClass{}		-> t'

		-- only substitute effect and closures if ld is turned on
		Just TSum{}	| not ld	-> tt
		Just TEffect{}	| not ld	-> tt		
		Just TFree{}	| not ld	-> tt
		Just TMask{}	| not ld	-> tt

		-- don't substitute for TBots or we risk loosing port vars
		Just TBot{}			-> tt

		-- hrm.. don't substitute for Effects and Closures when extracting un-generalised types.
		--	Or we risk loosing port vars in the substitution.
		--	Only Type.Scheme follows this codepath - but we should handle this a different way
		--	perhaps a flag to pack.
		Just t'				-- -> t'
			| k == KData		-> t'
			| otherwise		-> tt

		Nothing				-> tt

	TAccept t
	 -> let	t'	= packTypeLs ld ls t
	    in	TAccept t'

	-- sugar
	TMutable t
	 -> let t'	= packTypeLs ld ls t
	    in	TMutable t'
	    
	TElaborate t
	 -> let t'	= packTypeLs ld ls t
	    in	TElaborate t'

	_ -> panic stage
		$ "packTypeLs: no match for " % show tt
		    
{-
makeSub KEffect  t1 t2	= makeTSum KEffect  [t1, t2]
makeSub KClosure t1 t2	= makeTSum KClosure [t1, t2]
makeSub _	 t1 t2	= t2
-}




-- Keep repacking a TFetters until the number of Fetters in it stops decreasing 
--	(ie, find a fixpoint of restrictFs . packFettersLs) 
--
packTFettersLs 
	:: Bool
	-> Map Type Type
	-> Type 
	-> Type

packTFettersLs ld ls tt
 = {-# SCC "packTFettersLs" #-}
   case tt of
	TFetters fs (TError{})
	 -> tt

 	TFetters fs t
	 -> let	ls'	= Map.union 
	 			(Map.fromList [(t1, t2) | FLet t1 t2 <- fs])
				ls

		tPacked		= packTypeLs ld ls' t

		fsPacked	= map (packFetterLs ld ls') 
				$ map shortLoopsF
				$ fs

		-- erase trivial closures
		(tZapped, fsZapped)
				= mapAccumL (zapCoveredTMaskF ls') tPacked fsPacked
		
		-- inline TBots, and fetters that are only referenced once
		(tInlined, fsInlined)
				= inlineFs tZapped fsZapped

		fsSorted	= sortFs		
				$ restrictFs tInlined
				$ fsInlined
				
		tFinal		= addFetters fsSorted tInlined

	    in	if length fsSorted < length fs
	    	 then packTFettersLs ld ls tFinal
		 else tFinal

	_ -> tt


-- | Pack the type in the RHS of a TLet
--
packFetterLs 
	:: Bool 
	-> Map Type Type 
	-> Fetter 
	-> Fetter

packFetterLs ld ls ff
 = {-# SCC "packFetterLs" #-}
   case ff of
 	FLet t1 t2
	 -> FLet t1 (crushT (packTypeLs ld ls t2))

	FConstraint v ts
	 -> FConstraint v 
	 	(map (packTypeLs True ls) ts)

	_	
	 -> ff


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

-- InlineFs ----------------------------------------------------------------------------------------
-- | Inline Effect and Closure fetters which are only referenced once.
--	Also inline (t = TBot) fetters
--
inlineFs :: Type -> [Fetter] -> (Type, [Fetter])
inlineFs tt fs
 = {-# SCC "inlineFs1" #-}
   let	vsFree	= collectTClassVars tt
   
   	-- build a substitution from the let fetters
	sub	= Map.fromList	
		$ [(t1, t2)	
			| FLet t1 t2 <- fs
			, not $ elem t1 vsFree]

	-- substitute in the the RHSs
	subF ff
		= case ff of
			FLet  t1 t2 	 -> FLet  t1 (subTT sub t2)
			FMore t1 t2	 -> FMore t1 (subTT sub t2)
			FConstraint v ts -> FConstraint v (map (subTT sub) ts)
			_		 -> ff

	fs'	= map subF fs
   in	(tt, fs')


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
					FLet t1 t2 	-> FLet t1 (substituteTT sub t2)
					_		-> f)
			$ fs

		t'	= substituteTT sub t

	    in	addFetters fs' t'

	_	-> tt	     	


-- | Short circuit loops in individual fetters,
--	ie !e1 = !{ ... !e1}  => !e1 = !{ ... Bot }
--
--	resoning: 	the sum is a lub.
--			joining in a copy of itself isn't going to change its value.
--
shortLoopsF :: Fetter -> Fetter
shortLoopsF ff
 = {-# SCC "shortLoopsF" #-}
   shortLoopsF' ff

shortLoopsF' (FLet t1 t2)
	|  elem (kindOfType t1) [KRegion, KEffect, KClosure]
	=  let	bot	= TBot (kindOfType t1)
	   	t2'	= substituteTT (Map.singleton t1 bot) t2
	   in	FLet t1 t2'
	   
shortLoopsF' f	= f
	
-- Sort Fetters ------------------------------------------------------------------------------------
-- | Sort fetters so effect and closure information comes out first in the list.
--	Also sort known contexts so there's less jitter in their placement in interface files

sortFs :: [Fetter] -> [Fetter]
sortFs fs
 = let	isLetK k f
  	 = case f of
	 	FLet _ t2	-> kindOfType t2 == k
		FMore _ t2	-> kindOfType t2 == k
		_		-> False

	isCon v1 f
	 = case f of
	 	FConstraint v _
		 | v == v1	-> True
		_		-> False
		

	( [ fsData,   fsEffect, fsClosure
	  , fsConstT, fsMutableT
	  , fsConst,  fsMutable
	  , fsLazy,   fsDirect
	  , fsPure ], fsRest)	
		= partitionFs 
			[ isLetK KData, 	isLetK KEffect, 	isLetK KClosure
			, isCon primConstT,	isCon primMutableT
			, isCon primConst,	isCon primMutable
			, isCon primLazy,	isCon primDirect
			, isCon primPure ]
			fs
				
    in	   fsData 	++ fsEffect ++ fsClosure 
    	++ fsConstT 	++ fsMutableT
	++ fsConst	++ fsMutable
	++ fsLazy 	++ fsDirect
	++ fsPure 
	++ fsRest
    
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
	= ( substituteTT (Map.singleton t1 (TBot k)) tt
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
