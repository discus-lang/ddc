
module Module.ExportNew
	(makeInterface)
where
import Module.Interface
import Module.Scrape
import Type.Exp
import Type.Util.Kind
import DDC.Var	
import DDC.Main.Error
import DDC.Main.Pretty
import Data.Maybe
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Core.Glob	as C
import qualified Core.Exp	as C
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "Module.ExportNew"


-- | Construct a module interface from all this stuff.
--	Various pieces of information come from different analyses and stages in the compiler, 
--	and we collect it all together here to make it easier to write out the interface file.
--	Nothing should go into the `Interface` that doesn't need to be in the interface file.
--	The actual pretty printing happens in Module.Interface.Pretty.
--	
makeInterface
	:: Scrape
	-> Set Var		-- ^ Vars of things that that should not be exported.
				--	This will include vars of bindings created during lambda lifting.
	-> Map Var Var		-- ^ Map of value to type vars.
	-> Map Var Type		-- ^ Map of type vars to inferred source types.
	-> C.Glob
	-> Interface
	
makeInterface
	scrape
	vsNoExport
	mapValueToTypeVars
	sourceTypeTable
	coreGlob

	= Interface
	{ intModuleId		
		= scrapeModuleName scrape

	, intImportedModules	
		= Set.fromList $ scrapeImported scrape

	, intData		
		= Map.map getIntDataOfCoreTop
		$ C.globData coreGlob

	, intRegion		
		= Map.map getIntRegionOfCorePRegion
		$ C.globRegion coreGlob

	, intEffect		
		= Map.map getIntEffectOfCorePEffect
		$ C.globEffect coreGlob

	, intClass
		= Map.map getIntClassOfCorePClass
		$ C.globClass coreGlob
		
	, intClassDecl	
		= Map.map getIntClassDeclOfCorePClassDict
		$ C.globClassDict coreGlob	
	
	, intClassInst		= Map.empty
	, intProjDict		= Map.empty
	, intInfix		= Map.empty

	-- Only export bindings that aren't in the vsNoExport set.
	, intBind
	 	= Map.map (getIntBindOfCorePBind coreGlob mapValueToTypeVars sourceTypeTable)
		$ Map.filterWithKey (\k p -> not $ Set.member k vsNoExport )
		$ C.globBind coreGlob 
	}
	
	
-- | Convert a core `PData` to an `IntData`.
getIntDataOfCoreTop :: C.Top -> IntData
getIntDataOfCoreTop pp@C.PData{}
	= IntData
	{ intDataName		= C.topDataName pp
	, intDataSourcePos	= undefined
	, intDataCtors		= Map.map getIntDataCtorOfCoreCtorDef $ C.topDataCtors pp }


-- | Convert a core `CtorDef` to an `IntDataCtor`.
getIntDataCtorOfCoreCtorDef :: C.CtorDef -> IntDataCtor
getIntDataCtorOfCoreCtorDef def
	= IntDataCtor
	{ intDataCtorName	= C.ctorDefName def
	, intDataCtorSourcePos	= undefined
	, intDataCtorType	= C.ctorDefType def
	, intDataCtorTag	= C.ctorDefTag  def
	, intDataCtorFields	= C.ctorDefFields def }
	
	
-- | Convert a core `PRegion` to an `IntRegion`.
getIntRegionOfCorePRegion :: C.Top -> IntRegion
getIntRegionOfCorePRegion pp@C.PRegion{}
	= IntRegion
	{ intRegionName		= C.topRegionName pp
	, intRegionSourcePos 	= undefined
	, intRegionWitnessKinds	
		= Map.fromList
		$ [(v, let Just k = kindOfType t in k)
			| (v, t) <- C.topRegionWitnesses pp] }


-- | Convert a core `PEffect` to an `IntEffect`.
getIntEffectOfCorePEffect :: C.Top -> IntEffect
getIntEffectOfCorePEffect pp@C.PEffect{}
	= IntEffect
	{ intEffectName		= C.topEffectName pp
	, intEffectSourcePos	= undefined
	, intEffectKind		= C.topEffectKind pp }


-- | Convert a core `PClass` into an `IntClass`
getIntClassOfCorePClass :: C.Top -> IntClass
getIntClassOfCorePClass pp@C.PClass{}
	= IntClass
	{ intClassName		= C.topClassName pp
	, intClassSourcePos	= undefined
	, intClassSuper		= C.topClassSuper pp }


-- | Convert a core `PClassDict` into a `ClassDecl`.
getIntClassDeclOfCorePClassDict :: C.Top -> IntClassDecl
getIntClassDeclOfCorePClassDict pp@C.PClassDict{}
	= IntClassDecl
	{ intClassDeclName	= C.topClassDictName pp
	, intClassDeclSourcePos	= undefined
	, intClassDeclTyVars	= C.topClassDictParams pp
	, intClassDeclMembers	= Map.fromList $ C.topClassDictTypes pp}


-- | Convert a core `PBind` into an `IntBind`
getIntBindOfCorePBind 
	:: C.Glob 		-- ^ The core glob.
	-> Map Var Var		-- ^ Map of value vars to type vars.
	-> Map Var Type		-- ^ Table of source types.
	-> C.Top 
	-> IntBind

getIntBindOfCorePBind 
	glob 
	mapValueToTypeVars 
	sourceTypeTable 
	pp@C.PBind{}

	= IntBind
	{ intBindName		= C.topBindName pp
	, intBindSourcePos	= undefined
	, intBindSeaName	= Nothing

	-- The type table maps type vars to types, 
	--	so we first have to lookup the type var corresponding
	--	to the name of the binding.
	, intBindType		
		= let 	v	= C.topBindName pp
		
			vT	= fromMaybe (panic stage $ "getIntBindOfCoreBind: no type var for " % v)
				$ Map.lookup v mapValueToTypeVars

			Just t	= Map.lookup vT sourceTypeTable
		  in	t
		
	, intBindSeaType	= undefined }
		
		



