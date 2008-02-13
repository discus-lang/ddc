
-- | Closure trimming for core types.
module Core.Util.Trim
	( trimClosureT
	, trimClosureC )

where	

import Core.Util.Bits
import Core.Util.Pack
import Core.Plate.FreeVars
import Core.ReconKind
import Core.Exp

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Error
import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
stage	= "Core.Util.Trim"

	
-- | Trim the closure portion of this type
trimClosureT :: Type -> Type
trimClosureT tt = {-# SCC "trimClosureT" #-} trimClosureT2 tt

trimClosureT2 tt
  = let	tt'	= packT $ trimClosureT' tt
    in	if tt' == tt
    		then tt'
		else trimClosureT2 tt'

trimClosureT' tt		
 = case tt of
 	TFetters t fs	
	 -> let	vsBound	= Set.fromList
	 		$ catMaybes 
			$ map takeBoundVarF fs

	    in	makeTFetters t (catMaybes $ map (trimClosureT_f vsBound) fs)

	_		-> tt


takeBoundVarF ff
 = case ff of
 	FWhere v _	-> Just v
	FMore  v _	-> Just v

	
-- | Trim the closure in this binding
--	where the binding was on a type
trimClosureT_f :: Set Var -> Fetter -> Maybe Fetter
trimClosureT_f bound ff
 = case ff of
 	FWhere v t2
	 |  kindOfType t2 == KClosure
	 -> Just $ FWhere v (trimClosureC bound t2)

 	FMore v t2
	 |  kindOfType t2 == KClosure
	 -> Just $ FMore v (trimClosureC bound t2)

	_ -> Just ff


-- | Trim a closure down to its interesting parts
trimClosureC :: Set Var -> Closure -> Closure
trimClosureC bound cc = {-# SCC "trimClosureC" #-} trimClosureC2 bound cc

trimClosureC2 bound cc
 | KClosure	<- kindOfType cc
  = let cc'	= packT $ trimClosureC' bound cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC2 bound cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' bound cc
 = let down	= trimClosureC bound
   in case cc of
 	TVar{}			-> cc
	TVarMore{}		-> cc
	TClass{}		-> cc
	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map down
		$  flattenTSum cc

	TMask KClosure t1@(TVar k v) t2
	 | Set.member v bound
	 -> t1
	 
	 | otherwise
	 -> cc

	TMask KClosure t1 t2	
	 -> let	t1'	= trimClosureC bound t1
	    in	TMask KClosure t1' t2

	TFetters c fs
	 -> let	vsBound	= Set.fromList
	 		$ catMaybes 
			$ map takeBoundVarF fs

		bound'	= Set.union vsBound bound

	    in  makeTFetters
		 	(trimClosureC bound' c)
			(catMaybes $ map (trimClosureC_f bound') fs) 

	-- Erase the quantifier if the var is no longer free in the type
	TForall b k t		
	 -> if Set.member (varOfBind b) (freeVars t)
	 	then TForall b k (down t)
		else down t

	-- If this closure has no free variables
	--	then it is closed and can safely be erased.
	TFree v t
	 |  null $ filter (\v 	-> Var.nameSpace v /= Var.NameValue
	 			&& (not $ Var.isCtorName v)) 
	 	 $ Set.toList $ freeVars t		
	 -> TBot KClosure

	 | otherwise
	 -> TFree v $ trimClosureC_z bound t
	 
	TTag   v		-> cc
	 
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc


-- | Trim either a data or closure element of a closure
--	We need this dispatch because the right hand side of a 
--	TFree can be either data or more closure
--
trimClosureC_z :: Set Var -> Type -> Type
trimClosureC_z bound z
 = case kindOfType z of
 	KData		-> trimClosureC_t bound z
	KClosure	-> trimClosureC bound z


-- | Trim a data element of a closure.
trimClosureC_t :: Set Var -> Type -> Type
trimClosureC_t bound tt
 = case tt of
	-- Trim the fetters of this data
 	TFetters c fs
	 -> let	vsBound	= Set.fromList
	 		$ catMaybes 
			$ map takeBoundVarF fs

		bound'	= Set.union vsBound bound

	    in	makeTFetters (trimClosureC_t bound' c) (catMaybes $ map (trimClosureC_f bound') fs)

	-- Trim under foralls
	TForall b k t		
	 -> if Set.member (varOfBind b) (freeVars t)
	 	then TForall b k (trimClosureC_t bound t)
		else (trimClosureC_t bound t)

	-- Don't care about contexts
	TContext t1 t2
	 -> t2

	-- Only the closure portion of a function actually holds data
	TFunEC t1 t2 eff clo	
	 -> clo

	-- 
	TData v ts	-> tt
	TVar k v	-> tt

	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter of a closure
trimClosureC_f :: Set Var -> Fetter -> Maybe Fetter
trimClosureC_f bound ff
 = case ff of
	-- Only more closure information is interesting
 	FWhere v1 c2
	 |  kindOfType c2 == KClosure
	 -> Just $ FWhere v1 (trimClosureC bound c2)

 	FMore v1 c2
	 |  kindOfType c2 == KClosure
	 -> Just $ FMore v1 (trimClosureC bound c2)

	_ -> Nothing

	
