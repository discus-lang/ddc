
module	Desugar.Slurp.State
	( Annot1
	, Annot2
	, BindMode (..)
	, CSlurpM
	, CSlurpS  (..)
	, initCSlurpS )
where
import Util
import Shared.Pretty
import Shared.Exp
import Shared.Base
import Type.Exp
import Type.Error
import Desugar.Exp
import Shared.Var		(Var, VarBind, NameSpace(..))
import qualified Data.Set 	as Set
import qualified Data.Map 	as Map
import qualified Shared.Var 	as Var
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
	, stateGen		:: Map NameSpace VarBind

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
		[ (NameValue,	Var.XBind ("v" ++ Unique.typeConstraint) 0)
		, (NameType,	Var.XBind ("t" ++ Unique.typeConstraint) 0)
		, (NameRegion,	Var.XBind ("r" ++ Unique.typeConstraint) 0)
		, (NameEffect,	Var.XBind ("e" ++ Unique.typeConstraint) 0) 
		, (NameClosure,	Var.XBind ("c" ++ Unique.typeConstraint) 0)
		, (NameField,	Var.XBind ("f" ++ Unique.typeConstraint) 0) 
		, (NameClass,	Var.XBind ("w" ++ Unique.typeConstraint) 0) ]
		
	, stateDataDefs		= Map.empty
	, stateCtorType		= Map.empty
	, stateCtorFields	= Map.empty

	, stateTypesRequest	= Set.empty

	, stateSlurpDefs 	= Map.empty

	, stateVarType		= Map.empty }
				

