{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Abstract identifiers for Disciple source modules.
module DDC.Var.ModuleId
	(ModuleId (..))
where
import DDC.Main.Pretty

-- | Module identifiers
data ModuleId
	= 
	-- | No module information. This sometimes means that
	--   the variable is in the module currently being compiled.
	  ModuleIdNil

	-- | Absolute module name, of the form M1.M2.M3
	| ModuleId [String]
	deriving (Show, Eq, Ord)


-- | Pretty print a module id.
instance Pretty ModuleId PMode where
 ppr m
  = case m of
	ModuleIdNil	-> ppr "@ModuleNil"
  	ModuleId vs	-> "." %!% vs

