
-- | Trimming of closures
--	Inferred closure tend to contain a lot of information that isn't useful to the solver
--	or core IR. We can trim out a lot of this superfulous stuff.
--
--	We're only interested in data contructors.
--
--	We only need the closure part of functions:
--		ie   a -(%e1 $c1)> b
--		only the $c1 part can contain data.
--
--	Note: trimming under foralls, must retain quantification of some vars.
--
--		forall a %r1. a -($c1)> b
--		:- $c1 = Thing a %r1 %r2
--
--	reduce to
--		forall a %r1. Thing a %r1 %r2	

module Type.Closure.Trim
	( trimClosureT 
	, trimClosureC )
	
where

import Util
import Shared.Error
import Type.Exp
import Type.Plate
import Type.Util
import Type.Pretty
import Type.Plate.FreeVars
import Type.Util.Pack

import qualified Shared.Var		as Var
import qualified Shared.VarSpace	as Var
import qualified Shared.VarUtil		as Var
import Shared.Var			(Var)

import Debug.Trace

-----
stage	= "Type.Closure.Trim"


-- | Trim the closure portion of this type
trimClosureT :: Type -> Type
trimClosureT tt
  = let	tt'	= packType $ trimClosureT' tt
    in	if tt' == tt
    		then tt'
		else trimClosureT tt'

trimClosureT' tt		
 = case tt of
 	TFetters fs t	-> addFetters (catMaybes $ map trimClosureT_fs fs) t
	_		-> tt

	
-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_fs ::	Fetter	-> Maybe Fetter
trimClosureT_fs ff
 = case ff of
 	FLet c1 c2	
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC c2

	_ -> Just ff


-- | Trim a closure down to its interesting parts
trimClosureC :: Closure -> Closure
trimClosureC cc
 | KClosure	<- kindOfType cc
  = let cc'	= packClosure $ trimClosureC' cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC cc'

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

	TFetters fs c
	 -> addFetters 
	 	(catMaybes $ map trimClosureC_fs fs) 
	 	(trimClosureC c)

	TForall vks t		
	 -> TForall vks (trimClosureC t)

	-- If this closure element has no classids or free variables
	--	then it is closed and can safely be erased.
	TFree v t
	 |  collectClassIds t 	== []
	 ,  null $ filter (\v 	-> Var.nameSpace v /= Var.NameValue
	 			&& (not $ Var.isCtorName v)) 
	 	 $ freeVarsT t		
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
 	TFetters fs c		
	 -> addFetters (catMaybes $ map trimClosureC_fs fs) (trimClosureC_t c)

	-- Trim under foralls
	TForall vks t		
	 -> TForall vks (trimClosureC_t t)

	-- Only the closure portion of a function actually holds data
	TFun t1 t2 eff clo	
	 -> clo

	-- 
	TData v ts	-> tt
	TVar k v	-> tt
	TClass{}	-> tt

	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter of a closure
trimClosureC_fs :: Fetter -> Maybe Fetter
trimClosureC_fs ff
 = case ff of
 	FLet c1 c2	

	 -- more closure information
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC c2

	 -- effect information might be referenced in a type constructor
	 | kindOfType c1 == KEffect
	 -> Just $ FLet c1 c2

	_ -> Nothing
