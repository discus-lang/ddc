
module Module.Graph
	(dotModuleHierarchy)
where
import Util
import Shared.Pretty
import Main.IO
import Shared.Var			(ModuleId(..))
import qualified Data.Map		as Map
import qualified Data.Set		as Set


dotModuleHierarchy 
	:: ModuleId			-- root module.
	-> [String]			-- names of modules to recursively reject from graph.
	-> [ModuleId]			-- modules imported directly by the root module.
	-> Map ModuleId ImportDef 	-- map of recursive imports.
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
	:: [ModuleId]
	-> ModuleId -> [ModuleId] 
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
	:: [ModuleId]
	-> String
	-> Maybe ModuleId
	
lookupNamedModule ms name
	= find (\m -> name == pprStrPlain m) ms
		

-----
expandImports
	:: Map ModuleId ImportDef
	-> Set ModuleId
	-> [ModuleId]
	-> [ModuleId]
	
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


