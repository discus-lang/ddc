
module Module.ExportNew
	(makeInterface)
where
import Module.Interface
import Module.Scrape
import Type.Util.Kind	
import qualified Core.Glob	as C
import qualified Core.Exp	as C
import qualified Data.Set	as Set
import qualified Data.Map	as Map


makeInterface
	:: Scrape
	-> C.Glob
	-> Interface
	
makeInterface
	scrape
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

	, intEffect		= Map.empty
	, intClass		= Map.empty
	, intClassDecl		= Map.empty
	, intClassInst		= Map.empty
	, intProjDict		= Map.empty
	, intInfix		= Map.empty
	, intBind		= Map.empty }
	
	
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
	
	
-- | Convert a code `PRegion` to an `IntRegion`
getIntRegionOfCorePRegion :: C.Top -> IntRegion
getIntRegionOfCorePRegion pp@C.PRegion{}
	= IntRegion
	{ intRegionName		= C.topRegionName pp
	, intRegionSourcePos 	= undefined
	, intRegionWitnessKinds	
		= Map.fromList
		$ [(v, let Just k = kindOfType t in k)
			| (v, t) <- C.topRegionWitnesses pp] }






