module Type.Util.Finalise
	( finaliseT
	, finaliseF)
where
import Type.Exp
import Type.Builtin
import Type.Util.Bits
import Shared.VarPrim
import DDC.Main.Error
import DDC.Var
import qualified Type.Util.PackFast	as PackFast
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import Util

stage	= "Type.Util.Finalise"

-- | After all constraints are processed, 
--	completely unconstrained and unquantified effect and closure vars can
--	be replaced by bottoms, as they can never be instantiated to anything else.
--
finaliseT 
	:: Set Var	-- ^ The variables that have been quantified anywhere so far
	-> Bool		-- ^ whether to default unbound value tyvars to unit.
	-> Type		-- ^ the type to finalise
	-> Type		-- ^ finalised type

finaliseT bound def tt
 = let tt'	= toFetterFormT
		$ PackFast.packType 
		$ toConstrainFormT 
		$ finaliseT' bound def tt
   in  if tt == tt'
   	then tt
	else finaliseT bound def tt'


finaliseT' bound def tt
 = let down	= finaliseT' bound def
   in  case tt of
  	TForall b k t	
	 -> let	Just v	= takeVarOfBind b
		bound'	= Set.insert v bound
	 	t'	= finaliseT' bound' def t
	    in	TForall b k t'
	    
	TFetters t fs
	 -> let	
		vksMore	= Set.fromList
			$ catMaybes
			$ map takeBindingVarF fs
	
		-- prefer the already bound vars here
	 	bound'	= Set.union bound vksMore

	    	fs'	= map (finaliseF bound' def) fs
		t'	= finaliseT' bound' def t
	    in	TFetters t' fs'
	  
	TConstrain t (Constraints crsEq crsMore crsOther)
	 -> let	bound'	= Set.unions
			   	[ Set.fromList $ [v | TVar _ v <- Map.keys crsEq ]
			   	, Set.fromList $ [v | TVar _ v <- Map.keys crsMore ] 
				, bound ]
			
		crsEq'	  = Map.map (finaliseT' bound' def) crsEq
		crsMore'  = Map.map (finaliseT' bound' def) crsMore
		crsOther' = map (finaliseF bound' def) crsOther
		t'	  = finaliseT' bound' def t
	    in	TConstrain t' (Constraints crsEq' crsMore' crsOther')
		  	
	TSum  k ts	-> makeTSum k (map down ts)

	TCon{}		-> tt 
	TVar  k v
	 	| elem k [kEffect, kClosure]
		, not $ Set.member v bound	-> tBot k
	 
	 	| def
		, elem k [kValue]
		, not $ Set.member v bound	-> makeTData primTUnit k []
	 
		| otherwise			-> tt
			
	TApp t1 t2	-> TApp (down t1) (down t2)
	
	TEffect v ts	-> TEffect v (map down ts)
	TFree   v t	-> TFree v (down t)
	TDanger t1 t2	-> TDanger (down t1) (down t2)

	TClass{}	-> tt
	TError{}	-> tt
	
	_		-> panic stage
			$ "finaliseT: no match for " % tt


finaliseF 
	:: Set Var
	-> Bool
	-> Fetter
	-> Fetter 
	
finaliseF bound def ff
	| FWhere t1 t2			<- ff
	= FWhere t1 (down t2)

	| FMore t1 t2			<- ff
	= FMore t1 (down t2)

	| FConstraint v ts		<- ff
	= FConstraint v (map down ts)

	| FProj j v t1 t2		<- ff
	= FProj j v (down t1) (down t2)

	where down	= finaliseT bound def

