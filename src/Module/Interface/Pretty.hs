
module Module.Interface.Pretty
	()
where
import Module.Interface
import DDC.Main.Pretty
import DDC.Var
import DDC.Util.Doc
import Type.Exp
import Type.Pretty			(prettyTS)
import qualified DDC.Config.Version	as Config
import qualified Data.Map		as Map

instance Docable ModuleId Str
 where	doc x	= DLeaf (ppr x)

instance Docable (PrettyM mode) (PrettyM mode)
 where	doc x	= DLeaf x	

instance Docable Var Str
 where	doc x	= DLeaf (ppr x)

instance Docable Int Str
 where	doc i 	= DLeaf (ppr i)

instance Docable Type Str 
 where	doc t	= doc $ prettyTS $ t

instance Docable Kind Str 
 where	doc k	= doc $ ppr k


instance Docable Interface Str where
 doc int
	= DList
	[ DNode "ddc-version" 	
		(doc $ ppr $ Config.version)

	, DNode "module"	
		(doc $ intModuleId int) 

	, DNode "imported-modules"
		(doc $ intImportedModules int) 

	, DNode "data-types"
		(doc $ Map.elems $ intData int)

	, DNode "regions"
		(doc $ Map.elems $ intRegion int)
	]


instance Docable IntData Str where
 doc idata
  	= DNode (varName $ intDataName idata)
	$ DList
	[ DNode "ctors"		(doc $ Map.elems $ intDataCtors idata) ]

	
instance Docable IntDataCtor Str where
 doc def
	= DNode (varName $ intDataCtorName def)
   	$ DList
	[ DNode "tag" 	 	(doc $ intDataCtorTag  def)
	, DNode "type"		(doc $ intDataCtorType def)

	, if Map.null $ intDataCtorFields def
		then DBlank
		else (DNode "fields"	
			$ DList [DNode (varName f) (doc i)
				| (f, i) <- Map.toList $ intDataCtorFields def])
	]


instance Docable IntRegion Str where
 doc def
	= DNode (varName $ intRegionName def)
	$ DList
	[ DNode "witnesses"
		$ DList	[ DNode (varName v) (doc k)
				| (v, k) <- Map.toList $ intRegionWitnessKinds def]]
	


