
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
	, trimClosureC)
	
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

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Closure.Trim"

debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStr ss) x
		else x


-- | Trim the closure portion of this type
trimClosureT :: Set Type -> Type -> Type
trimClosureT quant tt
  = let	tt'	= packType $ trimClosureT' quant tt
    in	if tt' == tt
    		then tt'
		else trimClosureT quant tt'

trimClosureT' quant tt		
 = case tt of
 	TFetters fs t	
	 	-> addFetters (catMaybes $ map (trimClosureT_fs quant) fs) t

	_	-> tt

	
-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_fs :: Set Type -> 	Fetter	-> Maybe Fetter
trimClosureT_fs quant ff
 = case ff of
 	FLet c1 c2	
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC quant c2

	FMore c1 c2
	 | kindOfType c2 == KClosure
	 -> Just $ FMore c1 $ trimClosureC quant c2

	_ -> Just ff


-- | Trim a closure down to its interesting parts
trimClosureC :: Set Type -> Closure -> Closure
trimClosureC quant cc
 | KClosure	<- kindOfType cc
  = let cc'	= packClosure $ trimClosureC' quant cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC quant cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' quant cc
 = let down	= trimClosureC quant
   in  case cc of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar KClosure v
		| Set.member cc quant	-> TBot KClosure
		| otherwise		-> cc

	-- cids are never quantified so we always have to keep them.
	TClass{}		-> cc

	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map down
		$  flattenTSum cc

	TMask KClosure t1 t2	
	 -> let	t1'	= down t1
	    in	TMask KClosure t1' t2

	TFetters fs c
	 -> addFetters 
	 	(catMaybes $ map (trimClosureC_fs quant) fs) 
	 	(trimClosureC quant c)

	-- update quantifiers to not quantify over vars which have been trimmed out
	TForall vks t		
	 -> let quant'	= foldr Set.insert quant 
				[TVar k v | (v, k) <- vks]
		
	    in	trimClosureC quant' t
	     
	-- If this closure element has no classids or free variables
	--	then it is closed and can safely be erased.
	TFree tag t
	 -> case kindOfType t of
	 	KClosure 
		  -> TFree tag $ trimClosureC quant t

		_ -> makeTSum KClosure 
			$ map (TFree tag)
			$ trimClosureC_t quant t
			

	TTag   v		-> cc
	 
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc



-- | Trim a data element of a closure.
trimClosureC_t :: Set Type -> Type -> [Type]
trimClosureC_t quant tt
 = case tt of
	-- Trim the fetters of this data
 	TFetters fs c		
	 -> let	-- can't dicard 
	 	-- quant'	= foldl' slurpBoundF quant fs
	    in  map (\t -> addFetters (catMaybes $ map (trimClosureC_fs quant) fs) t)
		 	$ trimClosureC_t quant c

	-- Trim under foralls
	TForall vks t		
	 -> let	quant'	= foldr Set.insert quant
				[ TVar k v | (v, k) <- vks]

	    in	trimClosureC_t quant' t
	
	TSum k ts	-> catMap (trimClosureC_t quant) ts
	TMask k t1 t2	-> [TMask k (makeTSum k $ trimClosureC_t quant t1) t2]

	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar k v
		| Set.member tt quant	-> []
		| otherwise		-> [tt]
	
	TBot{}		-> []
	TTop{}		-> [tt]

	-- 
	TData v ts	-> catMap (trimClosureC_t quant) ts
	    
	-- Only the closure portion of a function actually holds data
	TFun t1 t2 eff clo	
	 -> trimClosureC_t quant clo

	TEffect{}	-> []
	TFree v t	-> [trimClosureC quant tt]
	
	-- classids are never quantified, so we always have to keep them.
	TClass{} -> [tt]


	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter of a closure
trimClosureC_fs :: Set Type -> Fetter -> Maybe Fetter
trimClosureC_fs quant ff
 = case ff of
 	FLet c1 c2	

	 -- more closure information
	 |  kindOfType c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC quant c2

	 -- effect information might be referenced in a type constructor
	 | kindOfType c1 == KEffect
	 -> Just $ FLet c1 c2

	FMore c1 c2
	 | kindOfType c2 == KClosure
	 -> Just $ FMore c1 $ trimClosureC quant c2

	 | kindOfType c2 == KEffect
	 -> Just $ FMore c1 c2

	_ -> Nothing


slurpBoundF :: Set Type -> Fetter -> Set Type
slurpBoundF quant ff
 = case ff of
 	FMore t1 _	-> Set.insert t1 quant
	FLet  t1 _	-> Set.insert t1 quant
	_		-> quant
