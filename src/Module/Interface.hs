module Module.Interface 
	( Interface 	(..)
	, IntData 	(..)
	, IntRegion 	(..)
	, IntEffect	(..)
	, IntClass	(..)
	, IntClassDecl	(..)
	, IntClassInst	(..)
	, IntProjDict	(..)
	, IntProj	(..)
	, IntInfix	(..)
	, IntBind	(..) )
where
import Shared.Base
import Shared.Var
import Type.Exp
import qualified Source.Exp	as S
import qualified Sea.Exp	as E
import Data.Map			(Map)
import Data.Set			(Set)


-- | A module interface.
--	This is enough information to use the code exported by the module.
--	TODO: We really want to keep this in some sort of binary database representation on disk,
--	      and avoid loading the entire thing when we only need part of the information.
--
data Interface
	= Interface
	{ -- | Id of the modue
	  intModuleId		:: ModuleId
		
	  -- | Other modules imported by this one.
	, intImportedModules	:: Set ModuleId

	  -- | Data types
	, intData		:: Map Var IntData
		
	  -- | Top level region decls
	, intRegion		:: Map Var IntRegion

	  -- | Effect decls
	, intEffect		:: Map Var IntEffect
	
	  -- | Class decls
	, intClass		:: Map Var IntClass

	  -- | Type Class Declarations
	, intClassDecl		:: Map Var IntClassDecl

	  -- | Type Class Instances
	, intClassInst		:: Map Var IntClassInst

	  -- | Projection Dictionaries
	, intProjDict		:: Map Var IntProjDict

	  -- | Associativity and precedence of infix operators
	, intInfix		:: Map Var IntInfix

	  -- | Top level value bindings
	, intBind		:: Map Var IntBind
	}
	deriving Show


-- | Interface to data types
data IntData
	= IntData
	{ intDataName		:: Var			-- ^ Name of data type
	, intDataSourcePos	:: SourcePos		-- ^ Where the data decl is
	, intDataCtor		:: Map Var Type	}	-- ^ Types of data constructors
	deriving Show
	

-- | Interface to a top level region
data IntRegion
	= IntRegion
	{ intRegionName		:: Var			-- ^ Region variable
	, intRegionSourcePos	:: SourcePos		-- ^ Where the region decl is
	, intRegionClass	:: Map Var Var }	-- ^ Region class -> witness var in Core IR
	deriving Show


-- | Interface to effect declaration
data IntEffect
	= IntEffect
	{ intEffectName		:: Var			-- ^ Effect constructor
	, intEffectSourcePos	:: SourcePos		-- ^ Where the effect decl is
	, intEffectKind		:: Kind	}		-- ^ Kind of effect constructor
	deriving Show
	

-- | Interface to abstract class declaration
data IntClass
	= IntClass
	{ intClassName		:: Var			-- ^ Class constructor
	, intClassSourcePos	:: SourcePos		-- ^ Where the class decl is
	, intClassSuper		:: Super }		-- ^ Superkind of class constructor
	deriving Show
	

-- | Interface to a type class	
data IntClassDecl
	= IntClassDecl
	{ intClassDeclName	:: Var			-- ^ Type class name (eg Show)
	, intClassDeclSourcePos	:: SourcePos		-- ^ Where the type class declaration is
	, intClassDeclTyVars	:: Map Var Kind		-- ^ Type variables and their kinds
	, intClassDeclMembers	:: Map Var Type }	-- ^ Types of member functions
	deriving Show
	
	
-- | Interface to a type class instance
data IntClassInst
	= IntClassInst
	{ intClassInstName	:: Var			-- ^ Class name    (eg Show)
	, intClassInstSourcePos	:: SourcePos		-- ^ Where the instance declaration is
	, intClassInstType	:: [Type]		-- ^ Instance type (eg Int)
	, intClassInstMembers	:: Map Var Var }	-- ^ Maps class member name to the binding
	deriving Show					--	that implements it.


-- | Interface to a projection dictionary
data IntProjDict
	= IntProjDict
	{ intProjDictTypeCtor	:: Var			-- ^ Type constructor the projections belong to
	, intProjTypes		:: Map Var IntProj }	-- ^ Map of projection name to info about it
	deriving Show


data IntProj
	= IntProj
	{ intProjName		:: Var			-- ^ Name of projection
	, intProjSourcePos	:: SourcePos		-- ^ Where the projection was defined
	, intProjType		:: Type			-- ^ Type of projection
	, intProjImpl		:: Var }		-- ^ Var of binding that implements this projection
	deriving Show


-- | Interface to an infix operator
data IntInfix
	= IntInfix
	{ intInfixName		:: Var			-- ^ Infix operator name
	, intInfixSourcePos	:: SourcePos		-- ^ Where the infix decl is
	, intInfixAssoc		:: S.InfixMode SourcePos -- ^ Associativity
	, intInfixPrec		:: Int }		-- ^ Precedence of operator
	deriving Show

								
-- | Interface to a top level binding
data IntBind
	= IntBind
	{ intBindName		:: Var			-- ^ Name of binding
	, intBindSourcePos	:: Var			-- ^ Location of binding
	, intBindSeaName	:: Maybe String		-- ^ Foreign Sea Name of binding (if any)
	, intBindType		:: Type 		-- ^ Type of binding
	, intBindSeaType	:: E.Type }		-- ^ Operational type of binding.
	deriving Show
