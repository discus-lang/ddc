
module DDC.Desugar.Elaborate.Constraint
	( KindSource(..)
	, Constraint(..))
where
import DDC.Main.Pretty
import DDC.Base.SourcePos
import DDC.Type
import DDC.Var


-- KindSource -------------------------------------------------------------------------------------
-- | Carries information about where a kind constraint came from.
data KindSource
	-- | Kind from the head of a data type definition
	= KSData SourcePos	

	-- | Kind from an effect definition
	| KSEffect SourcePos

	-- | Kind from a class definition
	| KSClass  SourcePos

	-- | Kind from a kind signature
	| KSSig  SourcePos
	deriving (Show)

instance Pretty KindSource PMode where
 ppr ks	= ppr $ show ks	


-- Constraint -------------------------------------------------------------------------------------
-- | A Kind constraint.
data Constraint 
	= Constraint KindSource Var Kind
	deriving (Show)
	
instance Pretty Constraint PMode where
 ppr (Constraint ks v k)	
 	= padL 20 v <> "::" <> padL 40 k <> parens ks % ";\n"
	

