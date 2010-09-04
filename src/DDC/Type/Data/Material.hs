{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Type.Data.Material
	(annotMaterialInDataDefs)
where
import DDC.Type.Data.Base
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Operators.Instantiate
import DDC.Var
import DDC.Main.Error
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "DDC.Type.Data.Material"

-- | Annotate some data type definitions with their sets of material vars.
annotMaterialInDataDefs
	:: Map Var DataDef	-- ^ Data defs in the environment. 
				--   We don't need to annotate these.
	-> Map Var DataDef	-- ^ Data defs that we need to annotate.
	-> Map Var DataDef	-- ^ Annotated data defs.

annotMaterialInDataDefs ddefsEnv ddefs
 	= Map.map (annotMaterialDataDef (Map.union ddefsEnv ddefs)) ddefs
		
		
-- | Annotate a data type definition with its set of material vars.
annotMaterialDataDef
	:: Map Var DataDef	-- ^ Data defs in the environment (including the one to annotate)
	-> DataDef		-- ^ Data def to annotate.
	-> DataDef		-- ^ Annotated data def

annotMaterialDataDef ddefsEnv ddef
 = let	tData	   = makeTData 
			(dataDefName ddef)
			(dataDefKind ddef)
			[TVar k (UVar v) | (v, k) <- dataDefParams ddef]

	vsMaterial = materialVarsOfType ddefsEnv tData
	
   in	ddef { dataDefMaterialVars	= Just vsMaterial }

	

-- | Determine which variables are material in this type.
--   This doesn't support quantified types. If there are any quantifers then `panic`.
materialVarsOfType 
	:: Map Var DataDef	-- ^ Map of data type constructor var to its definition.
	-> Type
	-> Set Var

materialVarsOfType dataDefs tt
 = go Set.empty [(emptyConstraints, tt)]

 where	go vsMaterial ds
	 = let	(vssMaterial', dss')	
			=  unzip
			$  map (materialVarsOfType1 dataDefs) ds

		vsMaterial'	= Set.unions vssMaterial'
		ds'		= concat dss'
		
	  in	if   (vsMaterial' == vsMaterial') 
		  && (ds          == ds')
			then vsMaterial
			else go vsMaterial' ds'
		

materialVarsOfType1
	:: Map Var DataDef	-- ^ Map of data type constructor var to its definition.
	-> (Constraints, Type)
	-> (Set Var, [(Constraints, Type)])

materialVarsOfType1 dataDefs (crs, tt)

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

		(TVar _ (UVar v1) : _)
		 -> let	
			-- If we've got a primary region variable then record is as material.
			vsMaterial	
			 | isRegionKind k	= Set.singleton v1
			 | otherwise		= Set.empty 
		
			-- look up the data definition for this type.
			Just dataDef	= Map.lookup vCon dataDefs 
		
			-- get the parameter of all the data constructors for this type.
			ctorParams	= concatMap quantParamsOfCtorType 
					$ map ctorDefType
					$ Map.elems 
					$ dataDefCtors dataDef

			tsParamsInst	= map	(\(Just t) -> t) 
					$ map	(flip instantiateT tsArgs)
						ctorParams

		    in	( vsMaterial
		    	, zip (repeat crs) tsParamsInst)
	
		_	-> panic stage $ "materialVarsOfType: no primary region var"
	
	| otherwise
	= (Set.empty, [])


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
