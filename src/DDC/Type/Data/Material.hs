
module DDC.Type.Data.Material
	( quantParamsOfCtorType
	, materialVarsOfType1)
where
import DDC.Core.Check.Type
import DDC.Type.Data.Base
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Var
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "DDC.Type.Data.Material"
	
-- | Get a list of all the parameters of a data constructor's type, retaining the outer quantifiers. 
--   This doesn't support constrained types. If there are any constraints then `panic`.
quantParamsOfCtorType :: Type -> [Type]
quantParamsOfCtorType t
	= quantParamsOfCtorType' [] [] t
	
quantParamsOfCtorType' bksQuant acc tt

	-- Remember quantified vars when we see them.
	| TForall b k t			<- tt
	= quantParamsOfCtorType'
		(bksQuant ++ [(b, k)])
		acc
		t

	-- We've got a function constructor, so add its param to the accumulator.
	-- Also wrap it with the current set of quantified vars.
	| Just (t1, t2, _, _)	<- takeTFun tt
	= quantParamsOfCtorType'
		bksQuant
		(makeTForall_front bksQuant t1 : acc)
		t2

	| otherwise
	= reverse acc
	
	

-- | Determine which variables are material in this type.
--   This doesn't support quantified types. If there are any quantifers then `panic`.
-- materialVarsOfType :: Type -> Set Var
-- materialVarsOfType tt


materialVarsOfType1
	:: Map Var DataDef
	-> Constraints 
	-> Type
	-> (Set Var, [Type])

materialVarsOfType1 dataDefs crs tt

	-- Plain region and value vars are always material.
	| TVar k b	<- tt
	, isRegionKind k || isValueKind k	
	, Just v	<- takeVarOfBound b
	= (Set.singleton v, [])

	-- Effect and closure vars are always immaterial
	| TVar k _	<- tt
	, isEffectKind k || isClosureKind k	
	= (Set.empty, [])
	
	-- 
	| Just (vCon, k, tsArgs) <- takeTData tt
	= case tsArgs of
		[]	-> (Set.empty, [])

		(TVar k1 (UVar v1) : tsRest)
		 -> let	Just dataDef	= Map.lookup vCon dataDefs 
		
			-- get the parameter of all the data constructors for this type.
			ctorParams	= concatMap quantParamsOfCtorType 
					$ map ctorDefType
					$ Map.elems 
					$ dataDefCtors dataDef
					
		    in	( Set.singleton v1
		    	, map 	(\t -> fst $ instantiateT (stage ++ "materialVarOfType1") t tsArgs) 
				ctorParams )
