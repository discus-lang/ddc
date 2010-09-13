{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

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
import Sea.Util
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Compounds
import DDC.Var
import Util

stage	= "DDC.Sea.Init"

-- | Add code that initialises this module
initTree
	:: ModuleId	-- ^ Name of this module.
	-> Tree () 	-- ^ Code for the module.
	-> Tree ()

initTree mid cTree
 = let 	
	-- Make code that initialises the top-level caf var.
	initCafSS	= catMap makeInitCaf 
			$ [ (v, t) 	| PCafSlot v t <- cTree
					, not (typeIsUnboxed t) ]

	-- The function to call to intialise the module.
	initV		= makeModuleInitVar mid
	super		= [ PProto initV [] TVoid
			  , PSuper initV [] TVoid initCafSS ]

   in	super ++ cTree


-- | Make code that initialises a CAF.
makeInitCaf :: (Var, Type) -> [Stmt ()]
makeInitCaf (v, t)
 = 	-- Allocate the slot at the top of the stack for this CAF.
	[ SAssign (XVar (NCafPtr v) ppObj) ppObj $ XVar nSlotPtr ppObj
	, SAssign xSlotPtr                 pObj  $ XPrim (MOp OpAdd) [xSlotPtr, xInt 1]

	-- Assign the new slot to zero, then call the function that computes the CAF.
	-- We want to set the slot to zero while we're computing the CAF incase
	-- it tries to recursively call itself.
	, SAssign (XVar (NCaf v) pObj) pObj      $ xInt 0
	, SAssign (XVar (NCaf v) pObj) pObj      $ XPrim (MApp $ PAppCall) [XVar (NSuper v) t] ]

 where	nSlotPtr	= NRts $ varWithName "_ddcSlotPtr"
	xSlotPtr	= XVar nSlotPtr ppObj
	pObj		= tPtrObj
	ppObj		= TPtr tPtrObj


-- | Make the var of the function we should use to initialise a module.
makeModuleInitVar :: ModuleId -> Var
makeModuleInitVar mid
 = case mid of
	ModuleId vs	-> varWithName ("ddcInitModule_" ++ (catInt "_" vs))
	_		-> panic stage $ "makeInitVar: no match"


-- Main -------------------------------------------------------------------------------------------
-- | Make code that initialises each module and calls the main function.
mainTree
	:: [ModuleId]	-- ^ list of modules in this program
	-> ModuleId	-- ^ The module holding the Disciple main function
	-> Tree ()

mainTree imports mainModule
 = let	ModuleId [mainModuleName]	= mainModule
   in	[ PMain mainModuleName
		$ map (\m -> "_" ++ (varName $ makeModuleInitVar m)) imports ]


