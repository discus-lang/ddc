module Type.Location
	( TypeSource 	(..)
	, SourceValue	(..)
	, SourceEffect	(..)
	, SourceClosure (..)
	, SourceUnify	(..)
	, SourceMisc	(..)
	, SourceInfer	(..)
	
	, takeSourcePos
	, dispSourcePos
	, dispTypeSource
	, dispSourceValue
	, dispFetterSource)
where
import Type.Util.Kind
import Type.Exp
import Shared.Pretty
import Shared.Base
import Shared.Literal
import Shared.Error
import Shared.VarPrim
import Util
import Shared.Var		(Var)
import qualified Shared.Var	as Var

-----
stage	= "Type.Location"

-- TypeSource --------------------------------------------------------------------------------------
-- Records where type constraints come from
--	These are used for producing nice error messages when a type error is found during inference

data TypeSource
	-- A dummy typesource for hacking around
	--	this shouldn't be present in deployed code.
	= TSNil	String
	
	-- These are positions from the actual source file.
	| TSV SourceValue			-- ^ Constraints on value types
	| TSE SourceEffect			-- ^ Constraints on effect types
	| TSC SourceClosure			-- ^ Constraints on closure types
	| TSU SourceUnify			-- ^ Why types were unified
	| TSM SourceMisc			-- ^ Other source of type things

	-- Things that were added to the graph by the type inferencer.
	| TSI SourceInfer
	deriving (Show, Eq)

instance Pretty TypeSource PMode where
 ppr (TSNil str) = "TSNil " % ppr str
 ppr (TSV sv)	= "TSV " % ppr sv
 ppr (TSE se)	= "TSE " % ppr se
 ppr (TSC sc)	= "TSC " % ppr sc
 ppr (TSU su)	= "TSU " % ppr su
 ppr (TSM sm)	= "TSM " % ppr sm
 ppr (TSI si)	= "TSI " % ppr si

 

-- | Sources of value constraints
data SourceValue
	= SVLambda	{ vsp :: SourcePos }			-- ^ Lambda expressions have function type
	| SVApp		{ vsp :: SourcePos }			-- ^ LHS of an application must be a function
	| SVLiteral	{ vsp :: SourcePos, vLit :: LiteralFmt } -- ^ Literal values in expressions have distinct types
	| SVDoLast	{ vsp :: SourcePos }			-- ^ Do expressions have the type of the last stmt.
	| SVIfObj	{ vsp :: SourcePos }			-- ^ Match object of an if-expression must be Bool	
	| SVProj	{ vsp :: SourcePos, vProj :: TProj }	-- ^ Value type constraints from field projection.
	| SVInst	{ vsp :: SourcePos, vVar :: Var }	-- ^ Type constraint from instance of this bound variable

	| SVLiteralMatch 
			{ vsp :: SourcePos, vLit :: LiteralFmt } -- ^ Matching against a literal value.

	| SVMatchCtorArg 
			{ vsp :: SourcePos } 			-- ^ Matching against a ctor gives types for its args.

	| SVSig		{ vsp :: SourcePos, vVar :: Var }	-- ^ Value constraint from type signature.
	| SVSigClass	{ vsp :: SourcePos, vVar :: Var }	-- ^ Signature from type-class dictionary definition.
	| SVSigExtern	{ vsp :: SourcePos, vVar :: Var }	-- ^ Signature from external type.

	| SVCtorDef	{ vsp :: SourcePos
			, vvarData :: Var
			, vvarCtor :: Var }			-- ^ Definition of constructor type.
	
	| SVCtorField	{ vsp :: SourcePos 
			, vVarData :: Var
			, vVarCtor :: Var 
			, vVarField :: Var }			-- ^ Type signature of constructor field. 


	deriving (Show, Eq)

instance Pretty SourceValue PMode where
 ppr (SVInst sp v)	= parens $ "SVInst" <> sp <> v
 ppr sv			= "SV " % vsp sv


-- | Sources of effect constraints
data SourceEffect
	= SEApp		{ esp :: SourcePos }			-- ^ Sum effects from application.
	| SEMatchObj	{ esp :: SourcePos }	 		-- ^ Effect from reading the match object.
	| SEMatch	{ esp :: SourcePos }			-- ^ Sum effects from match.
	| SEDo		{ esp :: SourcePos }			-- ^ Sum effects from do.
	| SEIfObj	{ esp :: SourcePos }			-- ^ Effect from reading the match object.
	| SEIf		{ esp :: SourcePos }			-- ^ Sum effects from if.
	| SEProj	{ esp :: SourcePos }			-- ^ Sum effects from projection.
	| SEGuardObj	{ esp :: SourcePos }			-- ^ Effect from reading the match object.
	| SEGuards	{ esp :: SourcePos }			-- ^ Sum effects from guards.
	deriving (Show, Eq)

instance Pretty SourceEffect PMode where
 ppr se		= "SE " % esp se


-- | Sources of closure constraints
data SourceClosure
	= SCApp		{ csp :: SourcePos }			-- ^ Sum closure from application.
	| SCLambda	{ csp :: SourcePos }			-- ^ lambda exprs bind their parameter var
	| SCMatch	{ csp :: SourcePos }			-- ^ Sum closure from match.
	| SCDo		{ csp :: SourcePos }			-- ^ Sum closure from do.
	| SCIf		{ csp :: SourcePos }			-- ^ Sum closure from if.
	| SCProj	{ csp :: SourcePos }			-- ^ Sum closure from projection.
	| SCGuards	{ csp :: SourcePos }			-- ^ Sum closure from guards.
	deriving (Show, Eq)

instance Pretty SourceClosure PMode where
 ppr sc		= "SE " % csp sc

-- | Sources of unification constraints
data SourceUnify
	= SUAltLeft	{ usp :: SourcePos }			-- ^ All LHS of case alternatives must have same type.
	| SUAltRight	{ usp :: SourcePos }			-- ^ All RHS of case alternatives must have same type.
	| SUIfAlt	{ usp :: SourcePos }			-- ^ All RHS of if expressions must have same type.
	| SUGuards	{ usp :: SourcePos }			-- ^ Unification of constraints placed on the match object by guards.
	| SUBind	{ usp :: SourcePos }			-- ^ LHS of binding has same type as RHS.
	deriving (Show, Eq)

instance Pretty SourceUnify PMode where
 ppr su		= "SU " % usp su

-- | Sources of other things (mostly to tag dictionaries)
data SourceMisc
	= SMGen		{ msp :: SourcePos, mVar :: Var }	-- ^ Used to tag source of generalisations due to let-bindings.
	| SMClassInst	{ msp :: SourcePos, mVar :: Var }	-- ^ Used to tag source of type-class instance dictionary definitions.
	| SMData	{ msp :: SourcePos } 			-- ^ Used to tag source of data definitions.
	| SMProjDict	{ msp :: SourcePos }			-- ^ Used to tag projection dictionaries.
	deriving (Show, Eq)


instance Pretty SourceMisc PMode where
 ppr sm		= "SM " % msp sm

-- | Sources of things added by the inferencer.
data SourceInfer
	-- | A name for an equivalence class invented by Type.Class.makeClassName
	= SIClassName

	-- | Used to tag const constraints arrising from purification of effects.
	| SIPurifier	
		ClassId		-- the class holding that effect and purity constraint
		Effect 		-- the effect that was purified
		TypeSource	-- the source of the effect
		Fetter		-- the original purity fetter
		TypeSource	-- the source of this fetter

	-- ^ The result of crushing some fetter, also carrying typeSource information
	--	This is used when crushing things like Shape constraints, where the node
	--	in the graph is deleted after crushing, so we can't look it back up if
	--	we hit an error involving it.
	| SICrushedFS
		ClassId
		Fetter
		TypeSource

	-- ^ The result of crushing some effect
	| SICrushedES	
		ClassId		-- the class holding the effect that was crushed
		Effect		-- the effect that was crushed
		TypeSource	-- the source of this effect

	-- ^ A scheme that was generalised and added to the graph because its 
	--	bound var was instantiated.
	| SIGenInst	Var

	-- ^ A scheme that was generalised and added to the graph during 
	--	the finialisation process of the solver.
	| SIGenFinal

	deriving (Show, Eq)

instance Pretty SourceInfer PMode where
 ppr SIClassName		= ppr "SIClassName"

 ppr (SIPurifier cid eff effSrc f fSrc)		
	= "SIPurifier" <> cid <> parens eff <> parens effSrc <> parens f <> parens fSrc

 ppr (SICrushedFS cid iF src)	= "SICrushedFS" <> cid <> parens iF <> src
 ppr (SICrushedES cid  iF src)	= "SICrushedES" <> cid <> parens iF <> src
 ppr (SIGenInst v)		= "SIGenInst " % v
 ppr SIGenFinal			= ppr "SIGenFinal"


---------------------------------------------------------------------------------------------------
takeSourcePos :: TypeSource -> Maybe SourcePos
takeSourcePos ts
 = case ts of
 	TSV sv	-> Just $ vsp sv
	TSE se	-> Just $ esp se
	TSC sc	-> Just $ csp sc
	TSU su	-> Just $ usp su
	TSM sm	-> Just $ msp sm
	
	TSI (SICrushedFS _ _ src)	-> takeSourcePos src
	TSI (SICrushedES _ _ src)	-> takeSourcePos src
	TSI (SIPurifier  _ _ _ _ fSrc)	-> takeSourcePos fSrc
		
	_	-> Nothing


dispSourcePos :: TypeSource -> PrettyM PMode
dispSourcePos ts
 = case takeSourcePos ts of
 	Just sp	-> ppr sp

	-- this shouldn't happen
	Nothing	
	 -> panic stage
		("dispSourcePos: no source location in " % ts)
		

-- Display -----------------------------------------------------------------------------------------
-- | These are the long versions of source locations that are placed in error messages

dispTypeSource :: Type -> TypeSource -> PrettyM PMode
dispTypeSource tt ts
	| TSV sv	<- ts
	= dispSourceValue tt sv

	| TSE se	<- ts
	= dispSourceEffect tt se
	
	| TSU su	<- ts
	= dispSourceUnify tt su

	| TSI (SICrushedFS c f ts') <- ts
	= dispTypeSource tt ts'

	| TSI (SICrushedES c eff effSrc) <- ts
	= dispTypeSource eff effSrc

	-- hrm.. this shouldn't happen
	| otherwise
	= panic stage
		("dispTypeSource: no match for " % ts % "\n")


-- A fn make showing error messages easier
--	whereas 'at' / 'with' refers to just a small part of it.
atKind :: Type -> String
atKind tt
	| Just k	<- kindOfType tt
	= let result
		| resultKind k == kValue	= "         at type: "
		| resultKind k == kEffect	= "     with effect: "
		| otherwise	= panic stage $ "atKind " % k
	  in result

	
-- | Show the source of a type error due to this reason
dispSourceValue :: Type -> SourceValue -> PrettyM PMode
dispSourceValue tt sv
 = case sv of
	SVLambda sp 	
		-> "lambda abstraction\n" 	
		%  "              at: " % sp	% "\n"

	SVApp sp	
		-> "  function application\n"
		%  "              at: " % sp	% "\n"
		

	SVLiteral sp lit
		-> "  literal value " % lit	% "\n"
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
		
	SVDoLast sp
		-> "  result of do expression\n"
		%  "              at: " % sp	% "\n"
		
		
	SVIfObj sp 
		-> "  object of if-then-else expression\n"
		%  "   which must be: Bool"	% "\n"
		%  "              at: " % sp	% "\n"
		
		
	SVProj sp j
	 -> let	cJ	= case j of
	 			TJField v	-> TJField  v { Var.nameModule = Var.ModuleNil }
	 			TJFieldR v	-> TJFieldR v { Var.nameModule = Var.ModuleNil }
	 			TJIndex v	-> TJIndex  v { Var.nameModule = Var.ModuleNil }
	 			TJIndexR v	-> TJIndexR v { Var.nameModule = Var.ModuleNil }

	    in	   "      projection '" % cJ % "'\n"
		%  atKind tt		% tt	% "\n"
		%  "              at: " % sp	% "\n"
	
	SVInst sp var
		-> "      the use of: " % var 	% "\n"
		%  atKind tt 		% tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVLiteralMatch sp lit
		-> "  match against literal value " % lit % "\n"
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVMatchCtorArg sp
		-> "  argument of constructor match\n" 
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVSig sp var
		-> "  type signature for '" % var % "'\n"
		%  "  which requires: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVSigClass sp var
		-> "  type signature for '" % var % "' in type-class definiton\n"
		%  "              at: " % sp	% "\n"
		
	SVSigExtern sp var
		-> "  type of import '" % var % "'\n"
		
	SVCtorDef sp vData vCtor
		-> "  definition of constructor '" % vCtor % "' of '" % vData % "'\n"
		%  "              at: " % sp	% "\n"
		
	SVCtorField sp vData vCtor vField
		-> "  definition of field " % vField % " of '" % vCtor % "' in type '" % vData % "'\n"
		%  "              at: " % sp	% "\n"

-- | Show the source of a type error due to this reason
dispSourceEffect :: Pretty tt PMode => tt -> SourceEffect -> PrettyM PMode
dispSourceEffect tt se
 = case se of
	SEApp sp
		-> "  effect from function application\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp 	% "\n"
		
	SEMatchObj sp
		-> "  effect due to testing match object\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp 	% "\n"
		
	SEMatch sp
		-> "  effect of match alternative\n"
		%  "          namely: " % tt	% "\n"
		%  "              at: " % sp 	% "\n"

	SEDo sp
		-> "  effect of do expression\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp 	% "\n"

	SEIfObj sp
		-> "  effect due to testing if-then-else discriminant\n"
		%  "              at: " % sp	% "\n"

	SEIf sp
		-> "  effect of if-then-else expression\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp 	% "\n"

	SEProj sp
		-> "  effect of field projection\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp	% "\n"
		
	SEGuardObj sp
		-> "  effect due to testing pattern guard\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp	% "\n"

	SEGuards sp
		-> "  effect of pattern guards\n"
		%  "          namely: " % tt 	% "\n"
		%  "              at: " % sp 	% "\n"



-- | Show the source of a type error due to this reason
dispSourceUnify :: Pretty tt PMode => tt -> SourceUnify -> PrettyM PMode
dispSourceUnify tt sv
 = case sv of
 	SUAltLeft sp 
	 	-> "alternatives"
		%  "              at: " % sp	% "\n"
		
	SUAltRight sp
		-> "result of alternatives"
		%  "              at: " % sp	% "\n"

	SUIfAlt sp
		-> "alternatives of if-then-else expression"
		%  "              at: " % sp	% "\n"

	SUGuards sp
		-> "match against pattern\n"
		%  "         at type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"

	SUBind sp
		-> ppr "binding"	



-- Normalise ---------------------------------------------------------------------------------------
--	We don't want to display raw classIds in the error messages for two reasons
--
--	1) Don't want to worry the user about non-useful information.
--	2) They tend to change when we modify the type inferencer, and we don't want to 
--		have to update all the check files for inferencer tests every time this happens.
--

-- Could do this directly on the string, if we displayed an * infront of type variables..


-- | Show the source of a fetter
--	This is used to print sources of class constraints when
--	we get a purity or mutability conflict.
--
--	The only possible source of these is instantiations of type schemes,
--	or from crushing other fetters.
--
dispFetterSource :: Fetter -> TypeSource -> PrettyM PMode
dispFetterSource f ts

	-- For purity constraints, don't bother displaying the entire effect
	--	purified, as most of it won't be in conflict.
	| FConstraint v _	<- f
	, v == primPure
	, TSV (SVInst sp var)	<- ts
	= "      the use of: " % var	% "\n"
	% "              at: " % sp	% "\n"
	
	| FConstraint v _	<- f
	, TSV (SVInst sp var)	<- ts
	= "      constraint: " % f	% "\n"
	% " from the use of: " % var	% "\n"
	% "              at: " % sp	% "\n"

	| TSV (SVInst sp var)	<- ts
	= "      constraint: " % f 	% "\n"
	% " from the use of: " % var	% "\n"
	% "              at: " % sp	% "\n"

	| TSV (SVSig  sp var) 	<- ts
	= "      constraint: " % f	% "\n"
	% " in type sig for: " % var	% "\n"
	% "              at: " % sp	% "\n"

	| FConstraint v _	<- f
	, TSI (SICrushedFS cid f' src)	<- ts
	= dispFetterSource f' src

	| FConstraint v _				<- f
	, TSI (SIPurifier _ eff effSrc fPure fPureSrc)	<- ts
	= "       constraint: " % f				% "\n"
	% "which purifies\n"
	% dispTypeSource   eff effSrc
	% "due to\n"
	% dispFetterSource fPure fPureSrc
	
	-- hrm.. this shouldn't happen
	| otherwise
	= panic stage 
		("dispFetterSource: no match for " % show ts % "\n")


