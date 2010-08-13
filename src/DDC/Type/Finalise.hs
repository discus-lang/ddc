{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Finalise
	( finaliseT_constrainForm)
where
import Shared.VarPrim
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Kind
import DDC.Var
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import Util

stage	= "Type.Util.Finalise"

-- | Replace unconstrained and unquantified effect and closure variables with bottoms.
--	Optionally, replace unconstrained and unquantified value type variables with
--	the unit type constructor.
--
finaliseT_constrainForm
	:: Set Var	-- ^ The variables that have been quantified so far.
	-> Bool		-- ^ Whether to also replace unconstrained value type variables with `Unit`.
	-> Type		-- ^ The type to finalise
	-> Type		-- ^ finalised type

finaliseT_constrainForm bound def tt
	= finaliseT' bound def tt

finaliseT' bound def tt
 = let down	= finaliseT' bound def
   in  case tt of
	TNil		-> panic stage $ "finaliseT: no match for TNil"

  	TForall b k t	
	 -> let	Just v	= takeVarOfBind b
		bound'	= Set.insert v bound
	 	t'	= finaliseT' bound' def t
	    in	TForall b k t'
	    
	TConstrain t (Constraints crsEq crsMore crsOther)
	 -> let	
		-- See what variable names are bound by constraints.
		bound'	= Set.unions
			   	[ Set.fromList $ [v | TVar _ (UVar v) <- Map.keys crsEq ]
			   	, Set.fromList $ [v | TVar _ (UVar v) <- Map.keys crsMore ] 
				, bound ]
			
		crsEq'	  = Map.filter (not . isTBot) $ Map.map (finaliseT' bound' def) crsEq
		crsMore'  = Map.filter (not . isTBot) $ Map.map (finaliseT' bound' def) crsMore
		crsOther' = map (finaliseF bound' def) crsOther

		-- Finalising the constraints above can rewrite the RHS's to bottom, 
		-- so we don't want to treat those as bound when finalising the body as well.
		bound''	= Set.unions
			   	[ Set.fromList $ [v | TVar _ (UVar v) <- Map.keys crsEq' ]
			   	, Set.fromList $ [v | TVar _ (UVar v) <- Map.keys crsMore' ] 
				, bound ]

		t'	  = finaliseT' bound'' def t

	    in	makeTConstrain t' (Constraints crsEq' crsMore' crsOther')
		  	
	TSum  k ts	-> makeTSum k (map down ts)

	TCon{}		-> tt 

	TVar k (UVar v)
	 	| isEffectKind k || isClosureKind k
		, not $ Set.member v bound	-> tBot k
	 
	 	| def
		, isValueKind k
		, not $ Set.member v bound	-> makeTData primTUnit k []
	 
		| otherwise			-> tt

	TVar{}		-> tt

	-- Finalising something like (x : $c1) leads to (x : ${}). 
	-- We detect this case and replace the whole thing with ${} so we don't have to 
	-- call the real packer.
	TApp t1 t2	
	 -> let	tt' = TApp (down t1) (down t2)
	    in  case takeTFree tt' of
	  	  Just (_, TSum k [])	-> TSum k []
		  _			-> tt'

	TError{}	-> tt
	


-- | Finalise a fetter.
--	If this returns `Nothing` then the RHS of the fetter worked out to be bottom
--	and we can remove it all-together.
finaliseF 
	:: Set Var	-- ^ The variables that have been quantified so far.
	-> Bool		-- ^ Whether to replace unconstrained value type variables with `Unit`.
	-> Fetter	-- ^ The fetter to finalise.
	-> Fetter	-- ^ The finalised fetter.
	
finaliseF bound def ff
 = let	down	= finaliseT' bound def
   in case ff of
	FWhere t1 t2		-> FWhere t1 (down t2)
	FMore  t1 t2		-> FMore  t1 (down t2)
	FConstraint v ts	-> FConstraint v (map down ts)
	FProj j v t1 t2		-> FProj j v (down t1) (down t2)


