
-- | Add code to initialise each module, and call the main function.
--   Initialising a module evaluates the values of the top-level CAFs.
--
--   TODO: As we know that CAFs are pure, we could suspended their evaluation
--         so there is no pause at startup time. This is especially important
--         if evaluation of one of the CAFs does not terminate.
--       
module DDC.Sea.Init
	( initTree
	, mainTree )
where
import Sea.Pretty
import Sea.Util
import DDC.Sea.Exp
import DDC.Var
import Util


-- | Add code that initialises this module
initTree
	:: ModuleId		-- ^ name of this module
	-> Tree () 		-- ^ code for the module
	-> Tree ()

initTree modName cTree
 = let 	initCafSS	= catMap makeInitCaf [ (v, t) | PCafSlot v t <- cTree, not (typeIsUnboxed t) ]
	initV		= makeInitVar modName
	super		= [ PProto initV [] TVoid
			  , PSuper initV [] TVoid initCafSS ]
   in	super ++ cTree


-- | Make code that initialises a CAF.
makeInitCaf :: (Var, Type) -> [Stmt ()]
makeInitCaf (v, t)
 = 	[ SAssign (xVarWithSeaName ("_ddcCAF_" ++  name) ppObj) ppObj slotPtr
	, SAssign slotPtr pObj (XPrim (MOp OpAdd) [slotPtr, XInt 1])
	, SAssign (XVarCAF v pObj) pObj (XInt 0)
	, SAssign (XVarCAF v pObj) pObj (XPrim (MApp $ PAppCall) [XVar v t]) ]
	where	name	= seaVar False v
		slotPtr = xVarWithSeaName "_ddcSlotPtr" ppObj
                pObj	= TPtr TObj
                ppObj	= TPtr pObj

makeInitVar (ModuleId vs)
	= varWithName ("ddcInitModule_" ++ (catInt "_" vs))

xVarWithSeaName name typ
 =	let v = Var
		{ varName 	= name
		, varModuleId	= ModuleIdNil
		, varNameSpace	= NameNothing
		, varId		= VarIdNil
		, varInfo	= [ISeaName name, ISeaGlobal True] }
	in XVar v typ


-- | Make code that initialises each module and calls the main function.
mainTree
	:: [ModuleId]		-- ^ list of modules in this program
	-> ModuleId		-- ^ The module holding the Disciple main function
	-> Tree ()

mainTree imports mainModule
 = let	ModuleId [mainModuleName]	= mainModule
   in	[ PMain mainModuleName
		$ map (\m -> "_" ++ (varName $ makeInitVar m)) imports ]


