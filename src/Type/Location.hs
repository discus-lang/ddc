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

import Type.Exp
import Type.Pretty
import Shared.Base
import Shared.Exp
import Shared.Literal
import Shared.Error

import Util

-----
stage	= "Type.Location"

-- TypeSource --------------------------------------------------------------------------------------
-- Records where type constraints come from
--	These are used for producing nice error messages when a type error is found during inference

data TypeSource
	-- These are positions from the actual source file.
	= TSV SourceValue			-- ^ Constraints on value types
	| TSE SourceEffect			-- ^ Constraints on effect types
	| TSC SourceClosure			-- ^ Constraints on closure types
	| TSU SourceUnify			-- ^ Why types were unified
	| TSM SourceMisc			-- ^ Other source of type things

	-- Things that were added to the graph by the type inferencer.
	| TSI SourceInfer
	deriving (Show, Eq)

-- | Sources of value constraints
data SourceValue
	= SVLambda	{ vsp :: SourcePos }			-- ^ Lambda expressions have function type
	| SVApp		{ vsp :: SourcePos }			-- ^ LHS of an application must be a function
	| SVLiteral	{ vsp :: SourcePos, vLit :: Const }	-- ^ Literal values in expressions have distinct types
	| SVDoLast	{ vsp :: SourcePos }			-- ^ Do expressions have the type of the last stmt.
	| SVIfObj	{ vsp :: SourcePos }			-- ^ Match object of an if-expression must be Bool	
	| SVProj	{ vsp :: SourcePos }			-- ^ Value type constraints from field projection.
	| SVInst	{ vsp :: SourcePos, vVar :: Var }	-- ^ Type constraint from instance of this bound variable

	| SVLiteralMatch 
			{ vsp :: SourcePos, vLit :: Const }	-- ^ Matching against a literal value.

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

-- | Sources of unification constraints
data SourceUnify
	= SUAltLeft	{ usp :: SourcePos }			-- ^ All LHS of case alternatives must have same type.
	| SUAltRight	{ usp :: SourcePos }			-- ^ All RHS of case alternatives must have same type.
	| SUIfAlt	{ usp :: SourcePos }			-- ^ All RHS of if expressions must have same type.
	| SUGuards	{ usp :: SourcePos }			-- ^ Unification of constraints placed on the match object by guards.
	| SUBind	{ usp :: SourcePos }			-- ^ LHS of binding has same type as RHS.
	deriving (Show, Eq)

-- | Sources of other things (mostly to tag dictionaries)
data SourceMisc
	= SMGen		{ msp :: SourcePos, mVar :: Var }	-- ^ Used to tag source of generalisations due to let-bindings.
	| SMClassInst	{ msp :: SourcePos, mVar :: Var }	-- ^ Used to tag source of type-class instance dictionary definitions.
	| SMData	{ msp :: SourcePos } 			-- ^ Used to tag source of data definitions.
	| SMProjDict	{ msp :: SourcePos }			-- ^ Used to tag projection dictionaries.
	deriving (Show, Eq)

-- | Sources of things added by the inferencer.
data SourceInfer
	= SIClassName						-- ^ A name for an equivalence class invented by Type.Class.makeClassName
	| SICrushed	{ ts :: TypeSource, iF :: Fetter}	-- ^ The result of crushing some fetter or effect
	| SIGenInst	Var					-- ^ A scheme that was generalised and added to the graph because its 
								--	bound var was instantiated.
	| SIGenFinal						-- ^ A scheme that was generalised and added to the graph during 
								--	the finialisation process of the solver.
	deriving (Show, Eq)


-- Pretty ------------------------------------------------------------------------------------------
instance Pretty TypeSource where
 ppr 	= ppr . show
 
instance Pretty SourceEffect where
 ppr 	= ppr . show
 
instance Pretty SourceValue where
 ppr	= ppr . show


---------------------------------------------------------------------------------------------------
takeSourcePos :: TypeSource -> Maybe SourcePos
takeSourcePos ts
 = case ts of
 	TSV sv	-> Just $ vsp sv
	TSE se	-> Just $ esp se
	TSC sc	-> Just $ csp sc
	TSU su	-> Just $ usp su
	TSM sm	-> Just $ msp sm
	
	TSI si	-> case si of
			SICrushed ts' _ -> takeSourcePos ts'
			_		-> Nothing


dispSourcePos :: TypeSource -> PrettyP
dispSourcePos ts
 = case takeSourcePos ts of
 	Just sp	-> ppr sp

	Nothing	-> panic stage
		$ "dispSourcePos: no source location in " % ts

-- Display -----------------------------------------------------------------------------------------
-- | These are the long versions of source locations that are placed in error messages


dispTypeSource :: Pretty tt => tt -> TypeSource -> PrettyP
dispTypeSource tt ts
	| TSV sv	<- ts
	= dispSourceValue tt sv
	
	| TSU su	<- ts
	= dispSourceUnify tt su
	
	| TSI si		<- ts
	, SICrushed ts'	_	<- si
	= dispTypeSource tt ts'
	
	| otherwise
	= panic stage
	$  "dispTypeSource: no match for " % ts % "\n"

	
-- | Show the source of a type error due to this reason
dispSourceValue :: Pretty tt => tt -> SourceValue -> PrettyP
dispSourceValue tt sv
 = case sv of
	SVLambda sp 	
		-> "lambda abstraction\n" 	
		%  "              at: " % sp	% "\n"

	SVApp sp	
		-> "function application\n"
		%  "              at: " % sp	% "\n"
		

	SVLiteral sp vLit 
		-> "literal value " % vLit	% "\n"
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
		
	SVDoLast sp
		-> "result of do expression\n"
		%  "              at: " % sp	% "\n"
		
		
	SVIfObj sp 
		-> "object of if-then-else expression\n"
		%  "   which must be: Bool"	% "\n"
		%  "              at: " % sp	% "\n"
		
		
	SVProj sp
		-> "field projection\n"
		%  "              at: " % sp	% "\n"
	
	SVInst sp var
		-> "          use of: " % var 	% "\n"
		%  "         at type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVLiteralMatch sp lit
		-> "match against literal value " % lit % "\n"
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVMatchCtorArg sp
		-> "argument of constructor match" 
		%  "         of type: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVSig sp var
		-> "type signature for '" % var % "'\n"
		%  "  which requires: " % tt	% "\n"
		%  "              at: " % sp	% "\n"
		
	SVSigClass sp var
		-> "type signature for '" % var % "' in type-class definiton\n"
		%  "              at: " % sp	% "\n"
		
	SVSigExtern sp var
		-> "type of import '" % var % "'\n"
		
	SVCtorDef sp vData vCtor
		-> "definition of constructor '" % vCtor % "' of '" % vData % "'\n"
		%  "              at: " % sp	% "\n"
		
	SVCtorField sp vData vCtor vField
		-> "definition of field " % vField % " of '" % vCtor % "' in type '" % vData % "'\n"
		%  "              at: " % sp	% "\n"


-- | Show the source of a type error due to this reason
dispSourceUnify :: Pretty tt => tt -> SourceUnify -> PrettyP
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


-- | Show the source of a fetter
--	This is used to print sources of class constraints when
--	we get a purity or mutability conflict.
--
--	The only possible source of these is instantiations of type schemes
--
dispFetterSource :: Fetter -> TypeSource -> PrettyP
dispFetterSource f ts
	| TSV sv		<- ts
	, SVInst sp var		<- sv
	= "      constraint: " % f 	% "\n"
	% "     from use of: " % var	% "\n"
	% "              at: " % sp	% "\n"
	
