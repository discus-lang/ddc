
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

module Type.Util.Trim
	( trimClosureT 
	, trimClosureC )
	
where

import Util
import Shared.Error
import Type.Exp
import Type.Plate
import Type.Pretty
import Type.Plate.FreeVars
import Type.Util.Pack
import Type.Util.Bits

import qualified Shared.Var		as Var
import qualified Shared.VarSpace	as Var
import qualified Shared.VarUtil		as Var
import Shared.Var			(Var)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Debug.Trace

-----
stage	= "Type.Closure.Trim"


-- | Trim the closure portion of this type
trimClosureT :: Set Type -> Type -> Type
trimClosureT bound tt
  = let	tt'	= packType $ trimClosureT' bound tt
    in	if tt' == tt
    		then tt'
		else trimClosureT bound tt'

trimClosureT' bound tt		
 = case tt of
 	TFetters fs t	
	 -> let	bound'	= foldl' slurpBoundF bound fs
	    in  addFetters (catMaybes $ map (trimClosureT_fs bound') fs) t
	_		-> tt

	
-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_fs :: Set Type -> 	Fetter	-> Maybe Fetter
trimClosureT_fs bound ff
 = case ff of
 	FLet c1 c2	
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC bound c2

	_ -> Just ff


-- | Trim a closure down to its interesting parts
trimClosureC :: Set Type -> Closure -> Closure
trimClosureC bound cc
 | KClosure	<- kindOfType cc
  = let cc'	= packClosure $ trimClosureC' bound cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC bound cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' bound cc
 = let down	= trimClosureC bound
   in  case cc of
 	TVar{}			-> cc
	TClass{}		-> cc
	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map down
		$  flattenTSum cc

	-- If there is no bound constraint for a closure var we can erase the mask
	--	(this is sound because it only ever makes the closure bigger)
	TMask KClosure t1@(TVar{}) t2
	 	| not $ Set.member t1 bound	-> down t1
	 	| otherwise			-> TMask KClosure (down t1) t2

	TMask KClosure t1@(TClass{}) t2
	 	| not $ Set.member t1 bound	-> down t1
	 	| otherwise			-> TMask KClosure (down t1) t2

	TMask KClosure t1 t2	
	 -> let	t1'	= down t1
	    in	TMask KClosure t1' t2

	TFetters fs c
	 -> addFetters 
	 	(catMaybes $ map (trimClosureC_fs bound) fs) 
	 	(trimClosureC bound c)

	-- update quantifiers to not quantify over vars which have been trimmed out
	TForall vks t		
	 -> let vsFree	= Set.toList $ freeVars t
		vks'	= [ (v, k)	| (v, k)	<- vks
					, elem v vsFree]
	    in	makeTForall vks' (down t)

	-- If this closure element has no classids or free variables
	--	then it is closed and can safely be erased.
	TFree v t
	 |  collectClassIds t 	== []
	 ,  null $ filter (\v 	-> Var.nameSpace v /= Var.NameValue
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
trimClosureC_z :: Set Type -> Type -> Type
trimClosureC_z bound z
 = case kindOfType z of
 	KData		-> trimClosureC_t bound z
	KClosure	-> trimClosureC bound z


-- | Trim a data element of a closure.
trimClosureC_t :: Set Type -> Type -> Type
trimClosureC_t bound tt
 = case tt of
	-- Trim the fetters of this data
 	TFetters fs c		
	 -> addFetters 
	 	(catMaybes $ map (trimClosureC_fs bound) fs) 
	 	(trimClosureC_t bound c)

	-- Trim under foralls
	TForall vks t		
	 -> let vsFree	= Set.toList $ freeVars t
		vks'	= [ (v, k)	| (v, k)	<- vks
					, elem v vsFree]
	    in	makeTForall vks' (trimClosureC_t bound t)

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
trimClosureC_fs :: Set Type -> Fetter -> Maybe Fetter
trimClosureC_fs bound ff
 = case ff of
 	FLet c1 c2	

	 -- more closure information
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC bound c2

	 -- effect information might be referenced in a type constructor
	 | kindOfType c1 == KEffect
	 -> Just $ FLet c1 c2

	FMore c1 c2
	 | kindOfType c2 == KClosure
	 -> Just $ FMore c1 $ trimClosureC bound c2

	 | kindOfType c2 == KEffect
	 -> Just $ FMore c1 c2

	_ -> Nothing


slurpBoundF :: Set Type -> Fetter -> Set Type
slurpBoundF bound ff
 = case ff of
 	FMore t1 _	-> Set.insert t1 bound
	FLet  t1 _	-> Set.insert t1 bound
	_		-> bound
