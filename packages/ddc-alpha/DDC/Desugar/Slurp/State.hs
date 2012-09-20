{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Type constraint slurper state.
module	DDC.Desugar.Slurp.State
	( Annot2
	, BindMode (..)
	, CSlurpM
	, CSlurpS  (..)
	, initCSlurpS
	, addDataDefToState
	, lookupDataDefOfCtorNamed
	, lookupCtorDefOfCtorNamed)
where
import DDC.Solve.Error
import DDC.Main.Pretty
import DDC.Solve.Interface.Problem
import DDC.Type
import DDC.Type.Data
import DDC.Var
import Util
import qualified Data.Set 	as Set
import qualified Data.Map 	as Map
import qualified Shared.Unique	as Unique

type	Annot2	= Maybe (Type, Effect)

-- | Expresses how a particular variable has been bound
--	At the moment we only differentiate between BindLet vs the rest during type inference.
--
--	The rest might be useful for error messages, and its easy to track this
--	info, so we do.
--
--	We'll probably need more of this information for working out the monomorphism restriction
--	when its type to implement type classing.
--
data	BindMode

	-- 'let bound'/scheme modes
	= BindLet			-- var is bound by a non-function let
	| BindExtern			-- var bound by import
	| BindCtor			-- var is a constructor function

	-- 'lambda bound'/non-scheme modes
	| BindLambda			-- var bound by lambda
	| BindSnoc			-- var bound by a pattern / deconstructor
	deriving (Show, Eq)

instance Pretty BindMode PMode where
 ppr xx	= ppr $ show xx


-- | State monad / state used by the constraint slurper.
type	CSlurpM	= State CSlurpS

data	CSlurpS
	= CSlurpS
     	{ stateTrace		:: [String]
	, stateErrors		:: [Error]

	-- | Variable generator.
	, stateGen		:: Map NameSpace VarId

	-- | Data type definitions.
	, stateDataDefs		:: Map Var DataDef

	-- | Map of constructor type names to data type names.
	, stateCtorData		:: Map Var Var

	-- | The set of TEC vars we need to infer TECs for so that we can
	--	convert the desugared code to core.
	, stateTypesRequest	:: Set Var

	-- | Maps value vars to type vars, v -> sigma_v
	, stateVarType		:: Map Var Var

	-- | Types defined via by a foreign import.
	, stateSlurpDefs	:: Map Var Type

	-- | Type signatures
	, stateSlurpSigs	:: Map Var [ProbSig]

	-- | Type class instances
	, stateSlurpClassInst	:: Map Var [ProbClassInst]

	-- | Projection dictionaries
	, stateSlurpProjDict	:: Map Var [ProbProjDict ] }


-- | The initial constraint slurper state.
initCSlurpS 	:: CSlurpS
initCSlurpS
	= CSlurpS
	{ stateTrace		= []

	, stateErrors		= []
	, stateGen
	   = 	Map.fromList
		[ (NameValue,	VarId ("v" ++ Unique.typeConstraint) 0)
		, (NameType,	VarId ("t" ++ Unique.typeConstraint) 0)
		, (NameRegion,	VarId ("r" ++ Unique.typeConstraint) 0)
		, (NameEffect,	VarId ("e" ++ Unique.typeConstraint) 0)
		, (NameClosure,	VarId ("c" ++ Unique.typeConstraint) 0)
		, (NameField,	VarId ("f" ++ Unique.typeConstraint) 0)
		, (NameClass,	VarId ("w" ++ Unique.typeConstraint) 0) ]

	, stateDataDefs		= Map.empty
	, stateCtorData		= Map.empty
	, stateTypesRequest	= Set.empty
	, stateVarType		= Map.empty
	, stateSlurpDefs 	= Map.empty
	, stateSlurpSigs	= Map.empty
	, stateSlurpClassInst	= Map.empty
	, stateSlurpProjDict	= Map.empty }


-- | Add a DatDef to the slurper state
addDataDefToState :: DataDef -> CSlurpS -> CSlurpS
addDataDefToState dataDef s
 = s	{ stateDataDefs	= Map.insert
				(dataDefName dataDef)
				dataDef
				(stateDataDefs s)

	, stateCtorData	= Map.union
				(stateCtorData s)
				(Map.fromList $ zip
					(Map.keys  $ dataDefCtors dataDef)
					(repeat    $ dataDefName  dataDef)) }


-- | Lookup the data type definition containing a given data constructor
lookupDataDefOfCtorNamed :: Var -> CSlurpM (Maybe DataDef)
lookupDataDefOfCtorNamed vCtor
 = do	ctorData	<- gets stateCtorData
	dataDefs	<- gets stateDataDefs

	return
	 $ do 	vData	<- Map.lookup vCtor ctorData
		Map.lookup vData dataDefs


-- | Lookup the data constructor definition for the constructor with this name.
lookupCtorDefOfCtorNamed :: Var -> CSlurpM (Maybe CtorDef)
lookupCtorDefOfCtorNamed vCtor
 = do	ctorData	<- gets stateCtorData
	dataDefs	<- gets stateDataDefs

	return
	 $ do	vData	<- Map.lookup vCtor ctorData
		dataDef	<- Map.lookup vData dataDefs
		Map.lookup vCtor $ dataDefCtors dataDef

