
module Type.Util.Pack
	( packType
	, sortFsT)

where

import Util
import Util.Graph.Deps

import Type.Exp
import Type.Plate.FreeVars
import Type.Plate.Trans
import Type.Plate.Collect
import Type.Util.Bits
import Type.Util.StripFetters

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))
import Shared.VarUtil		(sortForallVars)
import Shared.Error

import qualified Data.Map	as Map
import qualified Util.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Pack"


------------------------
-- packType
--	Pack a type into the standard form.	
--
--	* Do a full substition into the data portion of types.
--	* Erase fetters which aren't reachable from the type shape.
--	* Substitute TPure/TEmpty and crush TSums
--	* Erase TFrees if they contain no classids.
--
packType  :: Type -> Type
packType tt	
 = let	tt'	= packTypeLs [] tt
   in	if tt == tt'
   	 then tt'
	 else packType tt'
	 

packTypeLs :: [(Type, Type)] -> Type -> Type
packTypeLs ls tt
 = case tt of
 	TForall vks t	
	 -> let	t'	= packTypeLs ls t
	    in	TForall vks t'
	    
	TFetters fs t
	 -> packTFettersLs ls tt

	TUnify k ts
	 -> let ts'	= nub
	 		$ map (packTypeLs ls) 
			$ map (loadPureEmpty ls) ts
	    in	TUnify k ts'
	
	
	TSum KData ts
	 -> let ts'	= nub
	 		$ map (packTypeLs ls)
			$ map (loadFunData ls) ts
 	    in	makeTSum KData ts'
	 	 
	TSum k ts
	 -> let	ts'	= nub
	 		$ map (packTypeLs ls) 
			$ map (loadPureEmpty ls) ts
	    in	makeTSum k ts'

	
	    
	TMask k t1 t2
	 -> let	t1'	= packTypeLs ls t1
	 	t2'	= packTypeLs ls $ loadFunData ls t2
	    
	    in  makeTMask k t1' t2'
	    
	TVar{}	-> tt

	TTop k	-> tt
	TBot k 	-> tt
		 
	-- data
	TData v ts
	 -> let	ts'	= map ((packTypeLs ls) . (loadFunData ls)) ts
	    in	TData v ts'

 	TFun t1 t2 eff clo
	 -> let	t1'	= packTypeLs ls $ loadFunData ls t1
	 	t2'	= packTypeLs ls $ loadFunData ls t2
		eff'	= packTypeLs ls $ loadCV ls eff 
		clo'	= packTypeLs ls $ loadCV ls clo 
	    in	TFun t1' t2' eff' clo'

	-- effect
	TEffect v ts
	 -> let	ts'	= map ((packTypeLs ls) . (loadFunData ls)) ts
	    in	TEffect v ts'
	    
	-- closure
	TFree v t -> TFree v (loadFunData ls t)
	 
	TTag v	-> tt
	    
	-- wildcards
	TWild{}	-> tt

	-- solver
	--	substitute in trivial @cid1 = @cid2 constraints.
	TClass k cid1
	 -> case lookup tt ls of
	 	Just t@(TClass _ cid2)	-> t
		_			-> tt

	TAccept t
	 -> let	t'	= packTypeLs ls $ loadType ls t
	    in	TAccept t'

	-- sugar
	TMutable t
	 -> let t'	= packTypeLs ls $ loadType ls t
	    in	TMutable t'
	    
	TElaborate t
	 -> let t'	= packTypeLs ls $ loadType ls t
	    in	TElaborate t'

	_ -> panic stage
		$ "packTypeLs: no match for " % show tt
		    

-- Keep repacking a TFetters until the number of Fetters in it stops decreasing 
--	(ie, find a fixpoint of restrictFs . packFettersLs) 
--
packTFettersLs :: [(Type, Type)] -> Type -> Type
packTFettersLs ls tt
 = case tt of
 	TFetters fs t
	 -> let	ls'	= [(t1, t2) | FLet t1 t2 <- fs] ++ ls

		t'	= packTypeLs ls' 	
			$ loadType ls' t

		fs'	= sortFs		
			$ restrictFs t'
			$ inlineFs1 t'
			$ map (zapCoveredTMaskF ls') 
			$ map (packFetterLs ls') 
			$ map shortLoopsF
			$ fs

		tt'	= addFetters fs' t'

	    in	if length fs' < length fs
	    	 then packTFettersLs ls tt'
		 else tt'

	_ -> tt


-- | Pack the type in the RHS of a TLet
--
packFetterLs :: [(Type, Type)] -> Fetter -> Fetter
packFetterLs ls ff
 = case ff of
 	FLet t1 t2	-> FLet t1 (crushT (packTypeLs ls t2))
	_		-> ff


-- | Substitute for TClasses in this type.
loadType :: [(Type, Type)] -> Type -> Type
loadType ls tt
 = case tt of
 	TClass k cid
	 -> case lookup tt ls of
	 	Just t2	-> t2
		_	-> tt
		
	TVar k v
	 -> case lookup tt ls of
	 	Just t2	-> t2
		_	-> tt
		
	_	-> tt


loadFunData :: [(Type, Type)] -> Type -> Type
loadFunData ls tt
 = let	tt'	= loadType ls tt 
   in	case tt' of
		TSum KData _	-> tt'

   		TSum{}		-> tt
		TUnify{}	-> tt

		_		-> tt'

loadCV :: [(Type, Type)] -> Type -> Type
loadCV ls tt
 = let tt'	= loadType ls tt
   in  case tt' of
   		TClass{}	-> tt'
		TVar{}		-> tt'
		_		-> tt		



-- | Substitute TClasses for TPure or TEmpty in this type.
--
loadPureEmpty :: [(Type, Type)] -> Type -> Type
loadPureEmpty ls tt
 = case loadType ls tt of
	t@(TBot k)	-> t
	_		-> tt


-- | Restrict the list of TLet fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictFs :: Type -> [Fetter] -> [Fetter]
restrictFs tt ls
 = let	reachFLetsMap
 		= Map.fromList
		$ [(t, Set.fromList $ collectTClassVars tLet)	
 			| FLet t tLet	<- ls]
 
 	tsSeed		= Set.fromList $ collectTClassVars tt
	tsReachable	= tsSeed `Set.union` graphReachableS reachFLetsMap tsSeed
	 
   in	filter (\f -> case f of
			FLet t (TBot _)	-> False
   			FLet t _	-> Set.member t tsReachable
			_		-> True)
		$ ls


-- | Inline Effect and Closure fetters which are only referenced once.
--
inlineFs1 :: Type -> [Fetter] -> [Fetter]
inlineFs1 tt fs
 = let	
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
				FLet t1 t2 	-> FLet t1 (substituteTT sub t2)
				_		-> f)
		$ fs
		
 in 	fs'


-- | Short circuit loops in individual fetters,
--	ie !e1 = !{ ... !e1}  => !e1 = !{ ... Bot }
--
--	resoning: 	the sum is a lub.
--			joining in a copy of itself isn't going to change its value.
--
shortLoopsF :: Fetter -> Fetter
shortLoopsF (FLet t1 t2)
	|  elem (kindOfType t1) [KRegion, KEffect, KClosure]
	=  let	bot	= TBot (kindOfType t1)
	   	t2'	= substituteTT (Map.singleton t1 bot) t2
	   in	FLet t1 t2'
	   
shortLoopsF f	= f
	




-- | Sort fetters so effect and closure information comes out first in the list.
--
sortFs :: [Fetter] -> [Fetter]
sortFs fs
 = let 	isLetK k f
  	 = case f of
	 	FLet _ t2	-> kindOfType t2 == k
		_		-> False
		
	(fsData, fs2)		= partition (isLetK KData) 	fs
	(fsEffect, fs3)		= partition (isLetK KEffect) 	fs2
	(fsClosure, fs4)	= partition (isLetK KClosure)	fs3
	
    in	fsData ++ fsEffect ++ fsClosure ++ fs4
    
sortFsT :: Type -> Type
sortFsT tt
 = case tt of
 	TFetters fs t	-> TFetters (sortFs fs) t
	_		-> tt


-- | Erase TMasks where the LHS can only ever contain the values present in the RHS
-- eg
--	  @1 -(@4)> @2 -(@5)> @3
--        :- @4        = @5 \ f
--        ,  @5        = f :: ...
--
--	@5 is not substituted by inlineFs1 because it is referenced twice.
--	However, the binding for @4 is redundant because @5 can only ever contain f.
--	
-- rewrite to
--
--	  @1 -> @2 -(@5)> @3
--        :-  @5        = f :: ...
--
zapCoveredTMaskF :: [(Type, Type)] -> Fetter -> Fetter
zapCoveredTMaskF ls ff
	| FLet t1 (TMask k t2 t3)	<- ff
	, Just t2l			<- lookup t2 ls
	, coversCC t2l t3
	= FLet t1 (TBot k)

	| otherwise
	= ff
		 
coversCC (TFree v1 _) 	(TTag v2)	= v1 == v2
coversCC _		_		= False



