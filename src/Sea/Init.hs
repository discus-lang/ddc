
-- | Adds code to initialise each module, as well as the main program.
--	Initialising a module evaluates the values of each CAF.
--	This is done strictly atm, but we could suspend the evaluation of 
--	CAFs if we knew they were pure.
module Sea.Init
	( initTree
	, mainTree )
where
import Sea.Exp
import Sea.Pretty
import Sea.Util
import Util
import qualified Shared.Var	as Var
import Shared.Var		(Module(..))


-- | Add code that initialises this module
initTree 
	:: Module		-- ^ name of this module
	-> Tree () 		-- ^ code for the module
	-> Tree ()

initTree moduleName cTree
 = let 	initCafSS	= catMap makeInitCaf [ v | PCafSlot v t <- cTree, not (typeIsUnboxed t) ]
	initV		= makeInitVar moduleName
	super		= [ PProto initV [] TObj
			  , PSuper initV [] TObj initCafSS ]
   in	super ++ cTree

makeInitCaf v
 = 	[ SHackery ("     _ddcCAF_" ++  name ++ " = " ++ "_ddcSlotPtr++;")
	, SHackery ("     _CAF(" ++ name ++ ") = 0;")
	, SHackery ("     _CAF(" ++ name ++ ") = " ++ name ++ "();\n") ] 
	where	name	= seaVar False v
		 
makeInitVar (ModuleAbsolute vs)
	= Var.new ("ddcInitModule_" ++ (catInt "_" vs))


-- | Make code that initialises each module and calls the main function.
mainTree
	:: [Module]		-- ^ list of modules in this program
	-> Module		-- ^ The module holding the Disciple main function
	-> Tree ()
	
mainTree imports mainModule
 = let	ModuleAbsolute [mainModuleName]	= mainModule
	sLine	
	 = unlines $
		[ "int main (int argc, char** argv)"
		, "{"
		, "        _ddcRuntimeInit (argc, argv);" 
		, "" ]

		-- call all the init functions for imported modules.
	 ++ 	map (\m -> "\t" ++ "_" ++ (Var.name $ makeInitVar m) ++ "();") imports

		-- call the init function for the main module.
	 ++	[ "        _ddcInitModule_" ++ mainModuleName ++ "();" ]
		
		-- catch exceptions from the main function so we can display nice 
		--	error messages.
	 ++ 	[ ""
		, "        Control_Exception_topHandle(_allocThunk(" ++ mainModuleName ++ "_main, 1, 0));"
		, ""
		, "        _ddcRuntimeCleanup();"
		, "}"
		, ""
		]

   in	[PHackery sLine]

