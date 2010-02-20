module Type.Util.Finalise
	( finaliseT
	, finaliseF)

where
import Type.Exp
import qualified Type.Util.PackFast	as PackFast
import Type.Util.Bits
import Type.Util.Kind
import Shared.Error
import Shared.VarPrim

import Util
import qualified Shared.Var	as Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Data.Set			(Set)

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
	 -> let	bound'	= Set.insert (varOfBind b) bound
--	 		$ Map.fromList 
--			$ [(varOfBind b, (k, Nothing))]

	 	t'	= finaliseT' bound' def t
	    in	TForall b k t'
	    
	TFetters t fs
	 -> let	
{-		vksMore	= Map.fromList 
	 		$ map (\v -> (v, (kindOfSpace $ Var.nameSpace v, Nothing)))
	 		$ catMaybes 
			$ map takeBindingVarF fs
-}	 
		vksMore	= Set.fromList
			$ catMaybes
			$ map takeBindingVarF fs
	
		-- prefer the already bound vars here
	 	bound'	= Set.union bound vksMore

	    	fs'	= map (finaliseF bound' def) fs
		t'	= finaliseT' bound' def t
	    in	TFetters t' fs'
	  
{-	TConstrain t (Constraints crsEq crsMore crsOther)
	 -> let	vksEq	= Map.fromList 
			$ map (\(TVar k v) -> (v, (k, Nothing)))
			$ Map.keys crsEq

		vksMore	= Map.fromList
			$ map (\(TVar k v, t2) -> (v, (k, Just t2)))
			$ Map.toList crsMore
			
		bound 	= Map.unions [bound, vsMore, vksEq]
		
		fs'
			
			
			[(v, (kindOfSpace v, Nothing))
				| v	<- Map.keys crsEq]
-}	  
	TConstrain t crs
	 -> finaliseT' bound def
	 $  toFetterFormT tt
	
	TSum  k ts	-> makeTSum k (map down ts)

	TCon{}		-> tt 
	TVar  k v
	 	| elem k [kEffect, kClosure]
		, not $ Set.member v bound	-> TBot k
	 
	 	| def
		, elem k [kValue]
		, not $ Set.member v bound	-> makeTData primTUnit k []
	 
		| otherwise			-> tt
		
	TTop{}		-> tt
	TBot{}		-> tt
	
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

