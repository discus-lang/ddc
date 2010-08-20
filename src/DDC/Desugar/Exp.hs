{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Desugared Source IR.
module DDC.Desugar.Exp
	( Tree
	, Top		(..)
	, CtorDef	(..)
	, Exp		(..)
	, Proj		(..)
	, Stmt		(..)
	, Alt		(..)
	, Guard		(..)
	, Pat		(..)
	, Label		(..))
where
import DDC.Base.Literal
import DDC.Type
import DDC.Var.ModuleId
import DDC.Var
import Shared.Exp	(DataField)


-- | A desugared program tree is a list of top level declarations.
type Tree a	
	= [Top a]


-- | Top level declarations.
data Top a
	-- | Imported modules.
	= PImport 
		{ topAnnot		:: a 
		, topImportModules	:: [ModuleId] }
		
	-- | Import an external binding.
	| PExtern
		{ topAnnot		:: a
		, topExternVar		:: Var
		, topExternType		:: Type
		, topExternSeaType	:: Maybe Type }

	-- | Import an external data type.
	| PExternData	
		{ topAnnot		:: a
		, topExternDataSeaName	:: String
		, topExternDataVar	:: Var
		, topExternDataKind	:: Kind }

	-- | A super signature \/ abstract class constructor.
	| PSuperSig
		{ topAnnot		:: a
		, topSuperSigVar	:: Var
		, topSuperSigSuper	:: Super }

	-- | A kind signature  \/ abstract type constructor.
	| PKindSig
		{ topAnnot		:: a
		, topKindSigVar		:: Var
		, topKindSigKind	:: Kind }
		
	-- | Type signature.
	| PTypeSig	
		{ topAnnot		:: a
		, topTypeSigVars	:: [Var]
		, topTypeSigType	:: Type }

	-- | Type synonym.
	| PTypeSynonym
		{ topAnnot		:: a
		, topTypeSynonymVar	:: Var
		, topTypeSynonymType	:: Type }

	-- | Top level region declaration.
	| PRegion
		{ topAnnot		:: a
		, topRegionVar		:: Var }

	-- | Algebraic data type.
	| PData
		{ topAnnot		:: a
		, topDataName		:: Var
		, topDataParams		:: [Var]
		, topDataCtors		:: [CtorDef a] }

	-- | Data type class declaration.
	| PClassDecl	
		{ topAnnot		:: a
		, topClassDeclName	:: Var
		, topClassDeclParams	:: [Type]
		, topClassDeclMembers	:: [(Var, Type)] }

	-- | An instance for a type class.
	| PClassInst
		{ topAnnot		:: a
		, topClassInstCtor	:: Var
		, topClassInstArgs	:: [Type]
		, topClassInstMembers	:: [Stmt a] }

	-- | Projection dictionary.
	| PProjDict	
		{ topAnnot		:: a
		, topProjDictType	:: Type
		, topProjDictMembers	:: [Stmt a] }

	-- | Binding
	| PBind 
		{ topAnnot		:: a
		, topBindVar		:: Maybe Var
		, topBindExp		:: Exp a }
	deriving (Show, Eq)


-- | A data constructor definiton
data CtorDef a
	= CtorDef 
		{ ctorDefAnnot		:: a
		, ctorDefName		:: Var
		, ctorDefFields		:: [DataField (Exp a) Type] }
	deriving (Show, Eq)


-- | Expressions.
data Exp a
	= XNil

	-- The following constructors are accepted by the constraint slurper 
	
	-- | Void expression.
	| XVoid	 	
		{ expAnnot		:: a }

	-- | Variables.
	| XVar	{ expVarAnnot		:: a
		, expVarName		:: Var }

	-- | Literal values.
	| XLit 		
		{ expAnnot		:: a
		, expLitValue		:: LiteralFmt }

	-- | Projections.
	| XProj	
		{ expAnnot		:: a
		, expProjExp		:: Exp a
		, expProjProj		:: Proj a }

	-- | Direct use of a projection function, specifying the 
	--   type of the namespace to use.
	| XProjT
		{ expAnnot		:: a
		, expProjType		:: Type 
		, expProjProj		:: Proj a }

	-- | A lambda expression.
	| XLambda	
		{ expAnnot		:: a
		, expLambdaVar		:: Var
		, expLambdaExp		:: Exp a }

	-- | Function application.
	| XApp
		{ expAnnot		:: a
		, expAppFunExp		:: Exp a
		, expAppArgExp		:: Exp a }

	-- | Pattern matching.
	| XMatch
		{ expAnnot		:: a
		, expMatchObject	:: Maybe (Exp a)
		, expMatchAlts		:: [Alt a] }

	-- | Do expression, some statements to execute.
	| XDo
		{ expAnnot		:: a
		, expDoStmts		:: [Stmt a] }

	-- | if-then-else expression.
	| XIfThenElse
		{ expAnnot		:: a
		, expIfDiscrim		:: Exp a
		, expIfThen		:: Exp a
		, expIfElse		:: Exp a }


	-- The following constructors are produced by the constraint slurper
	-- but are not accepted by it.

	-- | Lambda expression tagged with variabes that give the
	--	type of the argument, and the effect and closure of the body.
	| XLambdaTEC
		{ expAnnot		:: a
		, expLambdaVar		:: Var
		, expLambdaExp		:: Exp a
		, expLambdaType		:: Type
		, expLambdaEffect	:: Effect
		, expLambdaClosure	:: Closure }


	-- | A projection tagged with the type and closure variables of
	--	the as-yet unknown projection function.
	| XProjTagged
		{ expProjAnnot		:: a

		-- | This type variable will be bound to the real type of the 
		--	projection function once we work out what it is.
		, expProjInstanceVar	:: Var 

		-- | This closure variable will be bound to the real closure
		--	of the projection function once we work out what it is.
		, expProjInstanceClosure :: Closure
		
		, expProjExp		:: Exp a
		, expProjProj		:: Proj a }


	-- | Like XProjTagged, but we don't need the expression
	| XProjTaggedT
	 	{ expAnnot		:: a
		, expProjInstanceVar	:: Var
		, expProjInstanceClosure :: Closure
		, expProjProj		:: Proj a }

	-- | An instance of a let-bound variable.
	--	We'll need to add type applications to this variable when
	--	we convert the program to the core language.
	| XVarInst
		{ expAnnot		:: a
		, expVarInstVar		:: Var }

	deriving (Show, Eq)
	

-- | Projections.
data Proj a
	= JField 	a Var
	| JFieldR	a Var
	deriving (Show, Eq)
	

-- | Statements.
data Stmt a
	= SBind 	a (Maybe Var) 	(Exp a)
	| SBindMonadic	a (Pat a) 	(Exp a)
	| SBindPat	a (Pat a) 	(Exp a)
	| SSig		a [Var]	Type
	deriving (Show, Eq)
	
	
-- | Case alternatives.
data Alt a
	= AAlt 		a [Guard a]	(Exp a)
	deriving (Show, Eq)
	
	
-- | Guards for alternatives.
data Guard a
	= GCase		a (Pat a)
	| GExp		a (Pat a)	(Exp a)
	deriving (Show, Eq)


-- | Patterns.
data Pat a
	= WConLabel	a Var		[(Label a, Var)]
	| WLit		a LiteralFmt

	-- Eliminated by Desugar.Patterns
	| WVar		a Var
	| WAt		a Var 		(Pat a)
	| WConLabelP	a Var 		[(Label a, Pat a)]
	deriving (Show, Eq)


-- | Labels for pattern matching.
data Label a
	= LIndex	a Int
	| LVar		a Var
	deriving (Show, Eq)

