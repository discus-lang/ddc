
module Module.ExportNew
	(makeInterface)
where
import Module.Interface
import Module.Scrape
import Type.Exp
import Type.Util.Kind
import DDC.Var	
import Data.Map			(Map)
import qualified Core.Glob	as C
import qualified Core.Exp	as C
import qualified Data.Set	as Set
import qualified Data.Map	as Map



makeInterface
	:: Scrape
	-> Map Var Var		-- ^ Map of value to type vars.
	-> Map Var Type		-- ^ Map of type vars to inferred source types.
	-> C.Glob
	-> Interface
	
makeInterface
	scrape
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
		
	, intClassDecl		= Map.empty
	, intClassInst		= Map.empty
	, intProjDict		= Map.empty
	, intInfix		= Map.empty

	, intBind
	 	= Map.map (getIntBindOfCorePBind coreGlob mapValueToTypeVars sourceTypeTable)
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


-- | Convert a core `PBind` into an `IntBind`
getIntBindOfCorePBind 
	:: C.Glob 
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

	, intBindType		
		= let Just vT	= Map.lookup (C.topBindName pp) mapValueToTypeVars
		      Just t	= Map.lookup vT sourceTypeTable
		  in  t
		
	, intBindSeaType	= undefined }
		
		



