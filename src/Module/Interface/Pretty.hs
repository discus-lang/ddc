
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

instance Docable (Doc Str) Str
 where	doc x	= x

instance Docable Var Str
 where	doc x	= DLeaf (ppr x)

instance Docable Int Str
 where	doc i 	= DLeaf (ppr i)

instance Docable Type Str 
 where	doc t	= doc $ prettyTS $ t

instance Docable Kind Str 
 where	doc k	= doc $ ppr k

instance Docable Super Str
 where	doc s	= doc $ ppr s

instance Docable Interface Str where
 doc int
	= DList
	[ DNode "ddc-version" 			$ doc $ ppr $ Config.version
	, DNode "module"			$ doc $ intModuleId int
	, dNodeIfElems "imported-modules"	$ intImportedModules int
	, dNodeIfElems "data-types" 		$ intData      int
	, dNodeIfElems "regions" 		$ intRegion    int
	, dNodeIfElems "effects"		$ intEffect    int
	, dNodeIfElems "classes"		$ intClass     int
	, dNodeIfElems "class-decls"		$ intClassDecl int
	, dNodeIfElems "class-insts"		$ intClassInst int
	, dNodeIfElems "proj-dicts"		$ intProjDict  int
	, dNodeIfElems "binds"			$ intBind      int
	]


instance Docable IntData Str where
 doc idata
  	= DNode (varName $ intDataName idata)
	$ DList [ dNodeIfElems "ctors"	$ intDataCtors idata ]

	
instance Docable IntDataCtor Str where
 doc def
	= DNode (varName $ intDataCtorName def)
   	$ DList
	[ DNode "tag" 	 	(doc $ intDataCtorTag  def)
	, DNode "type"		(doc $ intDataCtorType def)
	, dNodeIfElems "fields"
		[DNode (varName f) (doc i)
			| (f, i) <- Map.toList $ intDataCtorFields def]
	]


instance Docable IntRegion Str where
 doc def
	= DNode (varName $ intRegionName def)
	$ DList
	[ dNodeIfElems "witnesses"
		[ DNode (varName v) (doc k)
			| (v, k) <- Map.toList $ intRegionWitnessKinds def]]

	
instance Docable IntEffect Str where
 doc def
	= DNode (varName $ intEffectName def)
	$ DList
	[ DNode "kind"		(doc $ intEffectKind def) ]


instance Docable IntClass Str where
 doc def
	= DNode (varName $ intClassName def)
	$ DList
	[ DNode "super"		(doc $ intClassSuper def) ]
	
	
instance Docable IntClassDecl Str where
 doc def
	= DNode (varName $ intClassDeclName def)
	$ DList
	[ dNodeIfElems "params"
		[ DNode (varName v) (doc k)
			| (v, k) <- intClassDeclTyVars def ]

	, dNodeIfElems "members"
		$ map docMember $ Map.toList $ intClassDeclMembers def]

 	where
	 docMember (v, t)
		= DNode (varName v) $ DList [ DNode "type" (doc t) ]


instance Docable IntClassInst Str where
 doc def
	= DNode (varName $ intClassInstName def)
	$ DList
	[ dNodeIfElems "args"		$ intClassInstArgs def
	, dNodeIfElems "members"	
		[ DNode (varName v) (doc vInst)
			| (v, vInst)	<- Map.toList $ intClassInstMembers def ]
	]


instance Docable IntProjDict Str where
 doc def
	= DNode "dict"
	$ DList
	[ DNode "type"			(doc $ intProjDictType def) 
	, dNodeIfElems "members"	
		[ DNode (varName v) (doc vImpl)
			| (v, vImpl)	<- Map.toList $ intProjDictMembers def ] 
	]

	
instance Docable IntBind Str where
 doc def
	= DNode (varName $ intBindName def)
	$ DList
	[ DNode "type"		(doc $ intBindType def) ]
	
