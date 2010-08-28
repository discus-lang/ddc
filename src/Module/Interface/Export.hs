
module Module.Interface.Export
	(makeInterface)
where
import Module.Interface
import Module.Scrape
import DDC.Var
import DDC.Type
import DDC.Base.SourcePos
import DDC.Main.Error
import DDC.Main.Pretty
import Control.Monad
import Data.Maybe
import Data.Foldable
import Data.Set				(Set)
import Data.Map				(Map)
import Data.Sequence			(Seq)
import Prelude				hiding (foldl)
import qualified Source.Exp		as S
import qualified DDC.Desugar.Exp	as D
import qualified DDC.Core.Exp		as C
import qualified DDC.Type.Data		as T
import qualified DDC.Core.Glob		as C
import qualified Data.Set		as Set
import qualified Data.Map		as Map
import qualified Data.Sequence		as Seq
import qualified Util.Data.Map		as Map

stage	= "Module.Interface.Export"


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
	-> [S.Top SourcePos]	-- ^ Source    program tree.
	-> [D.Top SourcePos]	-- ^ Desugared program tree.
	-> C.Glob		-- ^ Core program glob.
	-> Interface
	
makeInterface
	scrape
	vsNoExport
	mpValueToTypeVars
	mpSourceTypes
	sourceTree
	desugaredTree
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
	
	, intClassInst		
		= Map.map (fmap getIntClassInstOfCorePClassInst)
		$ C.globClassInst coreGlob

	, intProjDict		
		= foldl addDesugaredProjDictToMap Map.empty 
		$ [p	| p@D.PProjDict{}	<- desugaredTree ]

	, intInfix
		= foldl addSourcePInfixToMap Map.empty
		$ [p	| p@S.PInfix{}		<- sourceTree ]

	-- Only export bindings that aren't in the vsNoExport set.
	, intBind
	 	= Map.map (getIntBindOfCorePBind coreGlob 
				mpValueToTypeVars mpSourceTypes)
		$ Map.filterWithKey (\k p -> not $ Set.member k vsNoExport )
		$ C.globBind coreGlob 
	}
	
	 
	
-- | Convert a core `PData` to an `IntData`.
getIntDataOfCoreTop :: C.Top -> IntData
getIntDataOfCoreTop pp@C.PData{}
	= IntData
	{ intDataName		= T.dataDefName  $ C.topDataDef pp
	, intDataSourcePos	= undefined
	, intDataCtors		= Map.map getIntDataCtorOfCoreCtorDef 
				$ T.dataDefCtors $ C.topDataDef pp }


-- | Convert a core `CtorDef` to an `IntDataCtor`.
getIntDataCtorOfCoreCtorDef :: T.CtorDef -> IntDataCtor
getIntDataCtorOfCoreCtorDef def
	= IntDataCtor
	{ intDataCtorName	= T.ctorDefName def
	, intDataCtorSourcePos	= undefined
	, intDataCtorType	= T.ctorDefType def
	, intDataCtorTag	= T.ctorDefTag  def
	, intDataCtorFields	= T.ctorDefFields def }
	
	
-- | Convert a core `PRegion` to an `IntRegion`.
getIntRegionOfCorePRegion :: C.Top -> IntRegion
getIntRegionOfCorePRegion pp@C.PRegion{}
	= IntRegion
	{ intRegionName		= C.topRegionName pp
	, intRegionSourcePos 	= undefined
	, intRegionWitnessKinds	
		= Map.fromList
		$ [(v, kindOfType t)
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


-- | Convert a core `PClassDict` into a `IntClassDecl`.
getIntClassDeclOfCorePClassDict :: C.Top -> IntClassDecl
getIntClassDeclOfCorePClassDict pp@C.PClassDict{}
	= IntClassDecl
	{ intClassDeclName	= C.topClassDictName pp
	, intClassDeclSourcePos	= undefined
	, intClassDeclTyVars	= C.topClassDictParams pp
	, intClassDeclMembers	= Map.fromList $ C.topClassDictTypes pp}


-- | Convert a core `PClassInst` to a `IntClassInst`.
getIntClassInstOfCorePClassInst pp@C.PClassInst{}
	= IntClassInst
	{ intClassInstName	= C.topClassInstName pp
	, intClassInstSourcePos	= undefined
	, intClassInstArgs	= C.topClassInstArgs pp
	, intClassInstMembers	
		= Map.fromList 
		$ C.topClassInstMembers pp
	}


-- | Convert a desugared `PProjDict` into an `IntProjDict`.
--	Projection dictionaries get desugared out before reaching 
--	the core program, so we have to get this info from the
--	desugared tree.
--	
addDesugaredProjDictToMap 
	:: Map Var (Seq IntProjDict) 
	-> D.Top a 
	-> Map Var (Seq IntProjDict)
	
addDesugaredProjDictToMap 
	mpProjDicts 
	p@(D.PProjDict _ typ members)

 = let	Just (vType, _, _)	= takeTData typ

	projDict	
		= IntProjDict
		{ intProjDictType	= typ
		, intProjDictMembers	= Map.fromList 
					$ map getIntProjOfDesugaredStmt 
					$ members 
		}
	
   in	Map.adjustWithDefault
		(Seq.|> projDict) Seq.empty
		vType mpProjDicts


getIntProjOfDesugaredStmt 
	:: D.Stmt a 			-- ^ Desugared stmt
	-> (Var, Var)

getIntProjOfDesugaredStmt 
	(D.SBind _ (Just vProj) (D.XVar _ vImpl))
 = 	(vProj, vImpl)


-- | Convert a source `PInfix` to an `IntInfix`.
addSourcePInfixToMap 
	:: Map Var IntInfix
	-> S.Top SourcePos
	-> Map Var IntInfix

addSourcePInfixToMap m p@(S.PInfix{})
	= Map.union m 
	$ Map.fromList
		[ (v, IntInfix 
			{ intInfixName		= v
			, intInfixSourcePos	= undefined
			, intInfixMode		= S.topInfixMode p
			, intInfixPrecedence	= S.topInfixPrecedence p })

			| v <- S.topInfixVars p ]
	

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



