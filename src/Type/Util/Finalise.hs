module Type.Util.Finalise
	( finaliseT
	, finaliseF)

where

import Type.Exp
import Type.Util.Pack
import Type.Util.Bits
import Shared.VarPrim
import Util

import qualified Data.Set	as Set
import Data.Set			(Set)


-- | After all constraints are processed, unbound effect and closure vars can be
--	replaced by bottoms.
--	
finaliseT 
	:: Set Var
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
	 -> let	bound'	= Set.union bound (Set.fromList $ map fst vks)
	 	t'	= finaliseT' bound' t
	    in	TForall vks t'
	    
	TFetters fs t
	 -> let	bound'	= Set.union bound (Set.fromList $ catMaybes $ map takeBindingVarF fs)
	    	fs'	= map (finaliseF bound') fs
		t'	= finaliseT' bound' t
	    in	TFetters fs' t'
	    
	TSum  k ts	-> makeTSum k (map down ts)
	TMask k t1 t2	-> makeTMask k (down t1) (down t2)

	TVar  k v
	 	| elem k [KEffect, KClosure]
		, not $ Set.member v bound	-> TBot k
	 
	 	| elem k [KData]
		, not $ Set.member v bound	-> TData primTUnit []
	 
		| otherwise			-> tt
		
	TTop{}			-> tt
	TBot{}			-> tt
	
	TData v ts		-> TData v (map down ts)
	TFun t1 t2 eff clo 	-> TFun (down t1) (down t2) (down eff) (down clo)
	
	TEffect v ts		-> TEffect v (map down ts)
	TFree   v t		-> TFree v (down t)
	TTag{}			-> tt

finaliseF bound ff
 = let down 	= finaliseT bound
   in  case ff of
   	FLet t1 t2		-> FLet t1 (down t2)
	FMore t1 t2		-> FMore t1 (down t2)
	FConstraint v ts	-> FConstraint v (map down ts)
	FProj j v t1 t2		-> FProj j v (down t1) (down t2)
