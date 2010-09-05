
module DDC.Desugar.Glob
	(Glob(..))
where
import DDC.Desugar.Exp
import DDC.Var
import Data.Sequence		(Seq)
import Data.Set			(Set)
import Data.Map			(Map)
--import qualified 
--import qualified Data.Set	as Set
--import qualified Data.Map	as Map

-- | A Glob provides a fast way to locate particular top level declarations.
--   Note: Don't add extra fields to this type that can't be reconstructed
-- 	   directly from a Tree. We want to be able to convert between
--	   Glob and Tree without losing information.	      
data Glob a
	= Glob
	{ 
	-- | Ids of modules imported by this one,	
	   globImports		:: Set ModuleId

	-- | Type sigs of imported things.
	, globExtern		:: Map Var (Top a)
	
	-- | Super signatures \/ abstract type class constructors.
	, globSuperSigs		:: Map Var (Top a)
	
	-- | Kind signatures \/ abstract type constructors.
	, globKindSigs		:: Map Var (Top a)
	
	-- | Type signatures.
	, globTypeSig		:: Map Var (Top a)
	
	-- | Type synonyms
	, globTypeSynonym	:: Map Var (Top a)
	
	-- | Top level region declarations
	, globRegion		:: Map Var (Top a)
	
	-- | Algebraic data type declarations
	, globData		:: Map Var (Top a)
	
	-- | Data type class declarations.
	, globClassDecl		:: Map Var (Top a)
	
	-- | Map of class name -> instances for that class
	, globClassInst		:: Map Var (Seq (Top a)) 
		
	-- | Projection dictionaries
	--   TODO: what's a better way to store these? by the outer type constructor?
	, globProjDict		:: [Top a]
	
	-- | Top level bindings
	, globBind		:: Map Var (Top a)
	} 
	deriving Show
	
	
	