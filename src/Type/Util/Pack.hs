module Type.Util.Pack
	( packType
	, packData
	, packEffect
	, packClosure
	, sortFsT
	, flattenT)

where

import Type.Exp
import Type.Pretty
import Type.Plate.Collect
import Type.Util.Bits

import Shared.VarPrim
import Shared.Error

import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Util.Map	as Map
import qualified Data.Set	as Set
import Util
import Util.Graph.Deps

import qualified Debug.Trace

-----
stage	= "Type.Util.Pack"
debug = True
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

	TSum k ts
	 -> let	ts'	= nub
	 		$ map (packTypeLs True ls) ts
	    in	makeTSum k ts'

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
	TFree v1 (TFree v2 t)		-> TFree v1 t
	TFree v1 (TBot KClosure)	-> TBot KClosure

	TFree v t -> TFree v (packTypeLs True ls t)
	 
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

		Just t'				-> t'
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
				= inlineFs1 tZapped fsZapped

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
		$ [(t, Set.fromList $ collectTClassVars tLet)	
 			| FLet t tLet	<- ls]
 
 	tsSeed		= Set.fromList 
			$ collectTClassVars tt
			++ concat [catMap collectTClassVars ts 
					| FConstraint v ts	<- ls]

	tsReachable	= tsSeed `Set.union` graphReachableS reachFLetsMap tsSeed
	 
   in	filter (\f -> case f of
			FLet t (TBot _)	-> False
   			FLet t _	-> Set.member t tsReachable
			_		-> True)
		$ ls


-- | Inline Effect and Closure fetters which are only referenced once.
--	Also inline (t = TBot) fetters
--
inlineFs1 :: Type -> [Fetter] -> (Type, [Fetter])
inlineFs1 tt fs
 = {-# SCC "inlineFs1" #-}
   let	
 	-- count how many times each var is used in the RHSs of the fs.
 	useCount
 		= Map.populationCount
		( (concat
 			$ map (\f -> case f of
 				FLet t1 t2	-> collectTClassVars t2
				_		-> [])
			$ fs)
		++ collectTClassVars tt)

		
	-- create a substitution containing the fs to substitute.
	--	always inline vars and cids,
	--	don't inline other types if they're used more than once, to prevent duplication of info in the type.
	
	sub	= Map.fromList
		$ catMaybes
		$ map (\f -> case f of
				FLet t1 t2@TVar{}	-> Just (t1, t2)
				FLet t1 t2@TClass{}	-> Just (t1, t2)
				FLet t1 t2	
				 |  Map.lookup t1 useCount == Just 1 	 -> Just (t1, t2)

				_ 					 -> Nothing)
		$ fs

	-- substitute in the the RHSs
	fs'	= map (\f -> case f of
				FLet t1 t2 	 -> FLet t1 (substituteTT sub t2)
				FConstraint v ts -> FConstraint v (map (substituteTT sub) ts)
				_		 -> f)
		$ fs
		
 in 	( tt
 	, fs')


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


flattenT :: Type -> Type
flattenT tt
 = flattenT' Map.empty Set.empty tt

flattenT' sub block tt
 = let down	= flattenT' sub block
   in  case tt of
   	TNil		-> TNil

	TForall vks t	-> TForall vks (down t)

	TFetters fs t
	 -> let (fsWhere, fsRest)
	 		= partition (=@= FLet{}) fs

		sub'	= Map.union 
				(Map.fromList $ map (\(FLet t1 t2) -> (t1, t2)) fsWhere)
				sub

		tFlat	= flattenT' sub' block t

	   in	addFetters fsRest tFlat

	TSum k ts	-> makeTSum  k (map down ts)
	TMask k t1 t2	-> makeTMask k (down t1) (down t2)

	TVar{}
	 | Set.member tt block
	 -> tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TClass{}
	 | Set.member tt block
	 -> tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TTop{}			-> tt
	TBot{}			-> tt

	TData v ts		-> TData v (map down ts)
	TFun t1 t2 eff clo	-> TFun (down t1) (down t2) (down eff) (down clo)

	TEffect v ts		-> TEffect v (map down ts)

	TFree v t		-> TFree v (down t)
	TTag v			-> TTag v

	TError{}		-> tt


