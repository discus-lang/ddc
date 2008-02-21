
module Module.Graph
	( dotModuleHierarchy )

where

import Util

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

----
import qualified Shared.Var		as Var
import Shared.Var			(Var, Module(..))
import Shared.Pretty

import Main.IO

-----
-- dotModuleHierarchy
--
dotModuleHierarchy 
	:: Module			-- root module.
	-> [String]			-- names of modules to recursively reject from graph.
	-> [Module]			-- modules imported directly by the root module.
	-> Map Module ImportDef 	-- map of recursive imports.
	-> String			-- module hierarchy graph in GraphViz format.

dotModuleHierarchy
	rootModuleName
	namesCut
	importsRoot
	importsExp

 = let	modulesCut	= [m | Just m 
 				<- map	(lookupNamedModule $ Map.keys importsExp) 
 					namesCut]

	modulesCutExp	= expandImports 
				importsExp 
				Set.empty 
				modulesCut 
	
	importsCut	= foldr Map.delete importsExp modulesCutExp
		
   in	"digraph G {\n"
	++ (dotImport modulesCutExp rootModuleName importsRoot)
	++ (catMap (\(m, def) 
			-> dotImport modulesCutExp m (idImportedModules def))
					
		$ Map.toList importsCut)
	++ "}\n"
 
-----
dotImport 
	:: [Module]
	-> Module -> [Module] 
	-> String

dotImport modulesCut m ms
	| elem m modulesCut	= ""
	| otherwise		
	= dotImportS m 
		[m | m <- ms, not $ elem m modulesCut]
	

dotImportS m ms
 = pprStrPlain
	$ "\t" 
	% (show $ pprStrPlain m)
	% " -> " 
	% "{" % " " %!% (map (show . pprStrPlain) ms) % "};\n"
 

-----
lookupNamedModule 
	:: [Module]
	-> String
	-> Maybe Module
	
lookupNamedModule ms name
	= find (\m -> name == pprStrPlain m) ms
		

-----
expandImports
	:: Map Module ImportDef
	-> Set Module
	-> [Module]
	-> [Module]
	
expandImports imports acc []	
	= Set.toList acc
	
expandImports imports acc (m:ms)
	| Set.member m acc
	= expandImports imports acc ms
	
	| Just int	<- Map.lookup m imports
	= expandImports 
		imports 
		(Set.insert m acc)
		(ms ++ idImportedModules int)


