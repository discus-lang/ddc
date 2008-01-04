
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
 	TWhere t vts	-> makeTWhere t (catMaybes $ map trimClosureT_vt vts)
	_		-> tt

	
-- | Trim the closure in this binding
--	where the binding was on a type
trimClosureT_vt ::	(Var, Type)	-> Maybe (Var, Type)
trimClosureT_vt vt
 = case vt of
 	(v, t2)
	 |  kindOfType t2 == KClosure
	 -> Just $  (v, trimClosureC t2)

	_ -> Just vt


-- | Trim a closure down to its interesting parts
trimClosureC :: Closure -> Closure
trimClosureC cc = {-# SCC "trimClosureC" #-} trimClosureC2 cc

trimClosureC2 cc
 | KClosure	<- kindOfType cc
  = let cc'	= packT $ trimClosureC' cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC2 cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' cc
 = case cc of
 	TVar{}			-> cc
	TClass{}		-> cc
	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map trimClosureC
		$  flattenTSum cc

	TMask KClosure t1 t2	
	 -> let	t1'	= trimClosureC t1
	    in	TMask KClosure t1' t2

	TWhere c vts
	 -> makeTWhere
	 	(trimClosureC c)
		(catMaybes $ map trimClosureC_vt vts) 

	-- Erase the quantifier if the var is no longer free in the type
	TForall b k t		
	 -> if Set.member (varOfBind b) (freeVars t)
	 	then TForall b k (trimClosureC t)
		else (trimClosureC t)

	-- If this closure has no free variables
	--	then it is closed and can safely be erased.
	TFree v t
	 |  null $ filter (\v 	-> Var.nameSpace v /= Var.NameValue
	 			&& (not $ Var.isCtorName v)) 
	 	 $ Set.toList $ freeVars t		
	 -> TBot KClosure

	 | otherwise
	 -> TFree v $ trimClosureC_z t
	 
	TTag   v		-> cc
	 
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc


-- | Trim either a data or closure element of a closure
--	We need this dispatch because the right hand side of a 
--	TFree can be either data or more closure
--
trimClosureC_z :: Type -> Type
trimClosureC_z z
 = case kindOfType z of
 	KData		-> trimClosureC_t z
	KClosure	-> trimClosureC z


-- | Trim a data element of a closure.
trimClosureC_t :: Type -> Type
trimClosureC_t tt
 = case tt of
	-- Trim the fetters of this data
 	TWhere c vts
	 -> makeTWhere (trimClosureC_t c) (catMaybes $ map trimClosureC_vt vts) 

	-- Trim under foralls
	TForall b k t		
	 -> if Set.member (varOfBind b) (freeVars t)
	 	then TForall b k (trimClosureC_t t)
		else (trimClosureC_t t)

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
trimClosureC_vt :: (Var, Type) -> Maybe (Var, Type)
trimClosureC_vt vt
 = case vt of
	-- Only more closure information is interesting
 	(v1, c2)
	 |  kindOfType c2 == KClosure
	 -> Just $ (v1, trimClosureC c2)

	_ -> Nothing

	
