
-- | Desugared Source IR.
module Desugar.Exp
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
import Type.Exp
import DDC.Base.Literal
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
	| PClassDict	
		{ topAnnot		:: a
		, topClassDictName	:: Var
		, topClassDictParams	:: [Type]
		, topClassDictMembers	:: [(Var, Type)] }

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


data Exp a
	= XNil

	-- Accepted by the constraint slurper.
	| XVoid	 	a
	| XLit 		a LiteralFmt
	| XVar	 	a Var
	| XProj		a (Exp a)  (Proj a)		
	| XProjT	a Type (Proj a)		
	| XLambda	a Var (Exp a) 
	| XApp		a (Exp a) (Exp a)
	| XMatch     	a (Maybe (Exp a)) [Alt a]
	| XDo       	a [Stmt a]
	| XIfThenElse	a (Exp a) (Exp a) (Exp a)

	-- Produced by the constraint slurper
	| XLambdaTEC 	a Var (Exp a) Type Effect Closure

	| XProjTagged	a 
		Var 		-- the instance variable for the projection function
				-- 	will be bound to its real type once we work it out

		Closure 	-- closure term of the projection function
				--	will be bound to the real closure once we work it out
		(Exp a) 

		(Proj a)

	-- Like XProjTagged, but we don't need the expression
	| XProjTaggedT  a Var Closure (Proj a)


	| XVarInst	a Var				-- An instance of a let bound variable
							--	We'll need to add TREC applications to this variable
							--	during Desugar->Core translation.

	deriving (Show, Eq)
	
	
data Proj a
	= JField 	a Var
	| JFieldR	a Var
	deriving (Show, Eq)
	

data Stmt a
	= SBind 	a (Maybe Var) (Exp a)
	| SBindMonadic	a (Pat a) (Exp a)
	| SBindPat	a (Pat a) (Exp a)
	| SSig		a [Var]	Type
	deriving (Show, Eq)
	

data Alt a
	= AAlt 		a [Guard a]	(Exp a)
	deriving (Show, Eq)
	
	
data Guard a
	= GCase		a (Pat a)
	| GExp		a (Pat a)	(Exp a)
	deriving (Show, Eq)

	
data Pat a
	= WConLabel	a Var [(Label a, Var)]
	| WLit		a LiteralFmt

	-- Eliminated by Desugar.Patterns
	| WVar		a Var
	| WAt		a Var (Pat a)
	| WConLabelP	a Var [(Label a, Pat a)]
	deriving (Show, Eq)


data Label a
	= LIndex	a Int
	| LVar		a Var
	deriving (Show, Eq)


