
module	Desugar.Slurp.State
	( Annot1
	, Annot2
	, BindMode (..)
	, CSlurpM
	, CSlurpS  (..)
	, initCSlurpS )
where
import Util
import Shared.Exp
import Type.Error
import Desugar.Exp
import DDC.Main.Pretty
import DDC.Base.SourcePos
import DDC.Type
import DDC.Var
import qualified Data.Set 	as Set
import qualified Data.Map 	as Map
import qualified Shared.Unique	as Unique


type	Annot1	= SourcePos
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

instance Pretty BindMode PMode
 where
 	ppr xx	= ppr $ show xx


-- | State monad / state used by the constraint slurper.
type	CSlurpM	= State CSlurpS
data	CSlurpS =
	CSlurpS 
     	{ stateTrace		:: [String]
	, stateErrors		:: [Error]

	-- Variable generator.
	, stateGen		:: Map NameSpace VarId

	, stateDataDefs		:: Map Var (Top Annot2)

	-- Types for constructors
	--	These are used to work out the types for corresponding patterns.
	, stateCtorType		:: Map Var Type					

	-- The fields in each constructor.
	, stateCtorFields	:: Map Var [DataField (Exp Annot2) Type]	

	-- The set of TEC vars we need to infer TECs for so that we can 
	--	convert the desugared code to core.
	, stateTypesRequest	:: Set Var

	, stateSlurpDefs	:: Map Var Type

	  -- maps value vars to type vars, v -> sigma_v
	, stateVarType		:: Map Var Var }
	
	

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
	, stateCtorType		= Map.empty
	, stateCtorFields	= Map.empty

	, stateTypesRequest	= Set.empty

	, stateSlurpDefs 	= Map.empty

	, stateVarType		= Map.empty }
				

