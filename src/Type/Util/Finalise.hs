module Type.Util.Finalise
	( finaliseT
	, finaliseF)

where
import Type.Exp
import Type.Util.Pack
import Type.Util.Bits
import Type.Util.Kind
import Shared.Error
import Shared.VarPrim

import Util
import qualified Shared.Var	as Var
import qualified Data.Map	as Map
import qualified Debug.Trace	as Debug

stage	= "Type.Util.Finalise"

-- | After all constraints are processed, unbound effect and closure vars can be
--	replaced by bottoms.
--	
finaliseT 
	:: Map Var (Kind, Maybe Type)
	-> Bool				-- whether to default unbound value tyvars to unit.
	-> Type
	-> Type

finaliseT bound def tt
 = let tt'	= packType_noLoops $ finaliseT' bound def tt
   in  if tt == tt'
   	then tt
	else finaliseT bound def tt'


finaliseT' bound def tt
 = let down	= finaliseT' bound def
   in  case tt of
  	TForall b k t	
	 -> let	bound'	= Map.union bound 
	 		$ Map.fromList 
			$ [(varOfBind b, (k, Nothing))]

	 	t'	= finaliseT' bound' def t
	    in	TForall b k t'
	    
	TFetters t fs
	 -> let	vksMore	= Map.fromList 
	 		$ map (\v -> (v, (kindOfSpace $ Var.nameSpace v, Nothing)))
	 		$ catMaybes 
			$ map takeBindingVarF fs
	 
		-- prefer the already bound vars here
	 	bound'	= Map.union bound vksMore

	    	fs'	= map (finaliseF bound' def) fs
		t'	= finaliseT' bound' def t
	    in	TFetters t' fs'
	    
	TSum  k ts	-> makeTSum k (map down ts)

	TCon{}		-> tt 
	TVar  k v
	 	| elem k [kEffect, kClosure]
		, not $ Map.member v bound	-> TBot k
	 
	 	| def
		, elem k [kValue]
		, not $ Map.member v bound	-> makeTData primTUnit k []
	 
		| otherwise			-> tt
		
	TTop{}			-> tt
	TBot{}			-> tt
	
	TApp t1 t2		-> TApp (down t1) (down t2)
	
	TEffect v ts		-> TEffect v (map down ts)
	TFree   v t		-> TFree v (down t)
	TDanger t1 t2		-> TDanger (down t1) (down t2)

	TClass{}		-> tt
	TError{}		-> tt
	
	_	-> panic stage
		$ "finaliseT: no match for " % tt

finaliseF bound def ff
	| FWhere t1@(TVar k v1) t2	<- ff
	, Just (k', Just tMore)		<- Map.lookup v1 bound
	, k 	== k'
	, tMore == t2
	= FMore t1 (down t2)

	| FWhere t1 t2			<- ff
	= FWhere t1 (down t2)

	| FMore t1 t2			<- ff
	= FMore t1 (down t2)

	| FConstraint v ts		<- ff
	= FConstraint v (map down ts)

	| FProj j v t1 t2		<- ff
	= FProj j v (down t1) (down t2)

	where down	= finaliseT bound def


	

	
	
	
