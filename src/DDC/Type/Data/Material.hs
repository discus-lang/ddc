{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Determine the material vars in a set of data type definitions.
--   BUGS: This will loop when given nested data types. 
--         We need to detect and explicitly reject them.
module DDC.Type.Data.Material
	(annotMaterialInDataDefs)
where
import DDC.Type.Collect.FreeVars ()
import DDC.Type.Data.Base
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Pretty		 ()
import DDC.Type.Operators.Instantiate
import DDC.Util.FreeVars
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import qualified Shared.VarUtil	as Var
import Data.List
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Debug.Trace

stage		= "DDC.Type.Data.Material"
debug		= False
trace ss xx	= if debug then Debug.Trace.trace (pprStrPlain ss) xx else xx

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

	vsMaterial   = milkVarsOfType (  materialVarsOfType1 ddefsEnv) tData
	vsImmaterial = milkVarsOfType (immaterialVarsOfType1 ddefsEnv) tData
	
   in	ddef 	{ dataDefMaterialVars	= Just vsMaterial 
		, dataDefImmaterialVars	= Just vsImmaterial }


-- Milk -------------------------------------------------------------------------------------------
type MilkFun
	=  (Constraints, Type)
	-> (Set Var, [(Constraints, Type)])

-- | Determine which variables are material in this type.
--   This doesn't support quantified types. If there are any quantifers then `panic`.
milkVarsOfType 
	:: MilkFun
	-> Type
	-> Set Var

milkVarsOfType milkFun tt
 = trace ("\nmilkVarsOfType: " % tt)
 $ go Set.empty [(emptyConstraints, tt)]

 where	go vs ds
	 = trace ("step " % (vs, map snd ds)) $ 
	   let	(vss', dss')	= unzip $ map milkFun ds
		vs'		= Set.unions (vs : vss')
		ds'		= nub $ concat dss'

		-- If no new vars or elems were addeed in this round then we're done.
	  in	if   (vs  == vs') 
		  && (ds  == ds')
			then vs
			else go vs' ds'



-- Material Vars ----------------------------------------------------------------------------------
-- | Do one step in the computation of material vars of a type.
--   TODO: This only works for ctor arg types at the moment, not arbitrary ones
--         that might include constraints or embedded effect and closure terms.
materialVarsOfType1
	:: Map Var DataDef	-- ^ Map of data type constructor var to its definition.
	-> (Constraints, Type)
	-> (Set Var, [(Constraints, Type)])

materialVarsOfType1 dataDefs (crs, tt)

	-- Plain region, value and closure vars are always material.
	| TVar k b	<- tt
	, isRegionKind k || isValueKind k || isClosureKind k
	, Just v	<- takeVarOfBound b
	= (Set.singleton v, [])

	-- Effect vars are always immaterial
	| TVar k _	<- tt
	, isEffectKind k
	= (Set.empty, [])
	
	-- 
	| Just (vCon, _, tsArgs) <- takeTData tt
	= case tsArgs of
		[]	-> (Set.empty, [])

		(TVar k1 (UVar v1) : _)
		 -> let	
			-- If we've got a primary region variable then record is as material.
			vsMaterial	
			 | isRegionKind k1	= Set.singleton v1
			 | otherwise		= Set.empty 
		
			-- look up the data definition for this type.
			Just dataDef	= Map.lookup vCon dataDefs 
		
			-- get the parameter of all the data constructors for this type.
			ctorParams	= concatMap quantParamsOfCtorType 
					$ map ctorDefType
					$ Map.elems 
					$ dataDefCtors dataDef

			Just tsParamsInst	
					= sequence
					$ map	(flip instantiateT tsArgs)
						ctorParams

		    in	( vsMaterial
		    	, zip (repeat crs) tsParamsInst)
	
		_	-> panic stage $ "materialVarsOfType: no primary region var"
	
	| Just (_, _, _, c1@(TVar _ (UVar vC1)))	<- takeTFun tt
	, isClosure c1
	= (Set.singleton vC1, [])
	

	| otherwise
	= panic stage $ "materialVarsOfType1: no match for " % tt


-- Immaterial Vars --------------------------------------------------------------------------------
-- | Do one step in the computation of immaterial vars of a type.
--   TODO: This only works for ctor arg types at the moment, not arbitrary ones
--         that might include constraints or embedded effect and closure terms.
immaterialVarsOfType1
	:: Map Var DataDef	-- ^ Map of data type constructor var to its definition.
	-> (Constraints, Type)
	-> (Set Var, [(Constraints, Type)])

immaterialVarsOfType1 dataDefs (crs, tt)

	-- Plain region, value and closure vars are always material.
	| TVar k _	<- tt
	, isRegionKind k || isValueKind k || isClosureKind k
	= (Set.empty, [])

	-- Effect vars are always immaterial
	| TVar k b	<- tt
	, isEffectKind k 
	, Just v	<- takeVarOfBound b
	= (Set.singleton v, [])
	
	-- 
	| Just (vCon, _, tsArgs) <- takeTData tt
	= let	-- look up the data definition for this type.
		Just dataDef = Map.lookup vCon dataDefs 
		
		-- get the parameter of all the data constructors for this type.
		ctorParams	= concatMap quantParamsOfCtorType 
				$ map ctorDefType
				$ Map.elems 
				$ dataDefCtors dataDef

		Just tsParamsInst	
			= sequence
			$ map	(flip instantiateT tsArgs)
				ctorParams

	  in	trace (vcat	[ "immaterial data " %% tt
				, "tsParamsInst = " %% tsParamsInst ])
		$ ( Set.empty
		  , zip (repeat crs) tsParamsInst)
	
	| Just (t1, t2, eff@TVar{}, _) <- takeTFun tt
	= let	vsImmaterial 	= Set.filter (not . Var.isCtorName)
				$ Set.unions $ map freeVars [t1, t2, eff]

	  in	trace ("immaterial fun " %% tt %% vsImmaterial)
		$ (vsImmaterial, [])

	| Just {} <- takeTFun tt
	= panic stage $ "immaterialVarsOfType1: no eff or closure var on function type"
	
	| otherwise
	= panic stage $ "immaterialVarsOfType1: no match for " % tt
	


-- | Get a list of all the parameters of a data constructor's type, 
--	retaining the outer quantifiers. 
quantParamsOfCtorType :: Type -> [Type]
quantParamsOfCtorType t
	= quantParamsOfCtorType' [] [] t
	
quantParamsOfCtorType' bksQuant acc tt
	| TConstrain t _	<- tt
	= quantParamsOfCtorType' bksQuant acc t

	-- Remember quantified vars when we see them.
	| TForall b k t		<- tt
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
