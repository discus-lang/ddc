module Type.Util.Finalise
	( finaliseT
	, finaliseF)

where

import Type.Exp
import Type.Util.Pack
import Type.Util.Bits
import Shared.VarPrim
import Util

import qualified Shared.Var	as Var

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace	as Debug

-- | After all constraints are processed, unbound effect and closure vars can be
--	replaced by bottoms.
--	
finaliseT 
	:: Map Var (Kind, Maybe Type)
	-> Type
	-> Type

finaliseT bound tt
 = let tt'	= packType $ finaliseT' bound tt
   in  if tt == tt'
   	then tt
	else finaliseT bound tt'


finaliseT' bound tt
 = let down	= finaliseT' bound
   in  case tt of
  	TForall vks t	
	 -> let	bound'	= Map.union bound 
	 		$ Map.fromList 
			$ map (\(v, k) -> (v, (k, Nothing))) vks

	 	t'	= finaliseT' bound' t
	    in	TForall vks t'
	    
	TFetters fs t
	 -> let	vksMore	= Map.fromList 
	 		$ map (\v -> (v, (kindOfSpace $ Var.nameSpace v, Nothing)))
	 		$ catMaybes 
			$ map takeBindingVarF fs
	 
		-- prefer the already bound vars here
	 	bound'	= Map.union bound vksMore

	    	fs'	= map (finaliseF bound') fs
		t'	= finaliseT' bound' t
	    in	TFetters fs' t'
	    
	TSum  k ts	-> makeTSum k (map down ts)
	TMask k t1 t2	-> makeTMask k (down t1) (down t2)

	TVar  k v
	 	| elem k [KEffect, KClosure]
		, not $ Map.member v bound	-> TBot k
	 
	 	| elem k [KData]
		, not $ Map.member v bound	-> TData primTUnit []
	 
		| otherwise			-> tt
		
	TTop{}			-> tt
	TBot{}			-> tt
	
	TData v ts		-> TData v (map down ts)
	TFun t1 t2 eff clo 	-> TFun (down t1) (down t2) (down eff) (down clo)
	
	TEffect v ts		-> TEffect v (map down ts)
	TFree   v t		-> TFree v (down t)
	TTag{}			-> tt

finaliseF bound ff
	| FLet t1@(TVar k v1) t2	<- ff
	, Just (k', Just tMore)		<- Map.lookup v1 bound
	, k 	== k'
	, tMore == t2
	= FMore t1 (down t2)

	| FLet t1 t2			<- ff
	= FLet t1 (down t2)

	| FMore t1 t2			<- ff
	= FMore t1 (down t2)

	| FConstraint v ts		<- ff
	= FConstraint v (map down ts)

	| FProj j v t1 t2		<- ff
	= FProj j v (down t1) (down t2)

	where down	= finaliseT bound

	

	
	
	
