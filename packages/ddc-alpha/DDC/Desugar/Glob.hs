
-- | A Glob an efficient way to organise top level declarations.
module DDC.Desugar.Glob
	( Glob(..)
	, globEmpty
	, insertTopInGlob
	, globOfTree
	, treeOfGlob)
where
import DDC.Desugar.Exp
import DDC.Type.Data
import DDC.Var
import Data.Map			(Map)
import qualified Data.Map	as Map

-- | A Glob provides an efficient way to organise top level declarations.
--   Note: Don't add extra fields to this type that can't be reconstructed
-- 	   directly from a Tree. We want to be able to convert between
--	   Glob and Tree without losing information.	      
data Glob a
	= Glob
	{ 
	-- | Ids of modules imported by this one,	
	   globImports		:: Map ModuleId a

	-- | Type sigs of imported things.
	, globExterns		:: Map Var (Top a)
	
	-- | Super signatures \/ abstract type class constructors.
	, globSuperSigs		:: Map Var (Top a)
	
	-- | Kind signatures \/ abstract type constructors.
	, globKindSigs		:: Map Var (Top a)
	
	-- | Type signatures.
	--   We have a list here because there could be multiple sigs
	--   for a given var, each with a different `SigMode`
	, globTypeSigs		:: Map Var [Top a]
	
	-- | Type synonyms
	, globTypeSynonyms	:: Map Var (Top a)
	
	-- | Top level region declarations
	, globRegions		:: Map Var (Top a)
	
	-- | Algebraic data type declarations
	, globDataDecls		:: Map Var (Top a)
	
	-- | Data type class declarations.
	, globClassDecls	:: Map Var (Top a)
	
	-- | Map of class name -> instances for that class
	--   TODO: store these more efficiently
	, globClassInsts	:: [Top a]
	
	-- | Projection dictionaries
	--   TODO: store these more efficiently
	, globProjDicts		:: [Top a]
	
	-- | Top level bindings
	, globBinds		:: Map Var (Top a)
	} 
	deriving Show
	

-- | An empty glob
globEmpty :: Glob a
globEmpty 
	= Glob
	{ globImports		= Map.empty
	, globExterns		= Map.empty
	, globSuperSigs		= Map.empty
	, globKindSigs		= Map.empty
	, globTypeSigs		= Map.empty
	, globTypeSynonyms	= Map.empty
	, globRegions		= Map.empty
	, globDataDecls		= Map.empty
	, globClassDecls	= Map.empty
	, globClassInsts	= []
	, globProjDicts		= []
	, globBinds		= Map.empty }
	
	
-- | Insert a top level thing into a glob.
--	If the top is already there the old one is updated.
insertTopInGlob :: Top a -> Glob a -> Glob a
insertTopInGlob pp glob
 = case pp of
	PImport nn mids	
	 -> glob { globImports    = Map.union (Map.fromList [(m, nn) | m <- mids]) (globImports glob) }

	PExtern{}
	 -> glob { globExterns	  = Map.insert (topExternVar pp)  pp (globExterns glob) }

	PSuperSig{}
	 -> glob { globSuperSigs  = Map.insert (topSuperSigVar pp) pp (globSuperSigs glob) }
			
	PKindSig{}
	 -> glob { globKindSigs	  = Map.insert (topKindSigVar pp)  pp (globKindSigs glob) }
			
	PTypeSig nn sigMode vs t
	 -> glob { globTypeSigs	  = Map.unionWith (++) 
						(Map.fromList [(v, [PTypeSig nn sigMode [v] t]) 
								| v <- topTypeSigVars pp])
					    	(globTypeSigs glob) }
	PTypeSynonym{}
	 -> glob { globTypeSynonyms
			= Map.insert (topTypeSynonymVar pp) pp (globTypeSynonyms glob)	}
				
	PRegion{}
	 -> glob { globRegions    = Map.insert (topRegionVar pp) pp (globRegions glob) } 

	PData{}
	 -> glob { globDataDecls  = Map.insert (dataDefName $ topDataDef pp) pp (globDataDecls glob) }
			
	PClassDecl{}
	 -> glob { globClassDecls = Map.insert (topClassDeclName pp) pp (globClassDecls glob) }
			
	PClassInst{}
	 -> glob { globClassInsts = pp : globClassInsts glob }
			
	PProjDict{}
	 -> glob { globProjDicts  = pp : globProjDicts glob }
	
	PBind{}
	 -> glob { globBinds      = Map.insert (topBindVar pp) pp (globBinds glob) }


-- | Convert a `Tree` to a `Glob`.
globOfTree :: Tree a -> Glob a
globOfTree tree
 	= foldr insertTopInGlob globEmpty tree


-- | Convert a `Glob` back to a `Tree`
--   TODO: We can delete this once all the desugar stages use globs.
--         We don't need the Tree back when converting to core.
treeOfGlob :: Glob a -> Tree a
treeOfGlob glob
	= [PImport n [mid]
		| (mid, n) <- Map.toList (globImports glob) ]

	++ (Map.elems
		$ Map.unions
		[ globExterns		glob
		, globSuperSigs		glob
		, globKindSigs		glob
		, globTypeSynonyms	glob
		, globRegions		glob
		, globDataDecls		glob
		, globClassDecls	glob])
	++ (concat $ Map.elems $ globTypeSigs   glob)
	++ globClassInsts glob
	++ globProjDicts  glob
	++ (Map.elems $ globBinds glob)
