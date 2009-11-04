
module Sea.Init
	( initTree
	, mainTree )

where

import Util
import qualified Shared.Var	as Var
import Shared.Var		(Module(..))

import Sea.Exp
import Sea.Pretty
import Sea.Util

import Debug.Trace as Debug

initTree 
	:: Module		-- name of this module
	-> Tree () 		-- source tree
	-> Tree ()

initTree moduleName cTree
 = let 	initAtomSS	= [ SAssign (XAtom v) t $ XAllocDataAnchored v 0
				| PAtom v t	<- cTree]

	initCafSS	= catMap makeInitCaf [ v | PCafSlot v t <- cTree, not (typeIsUnboxed t) ]
	initSS		= initAtomSS ++ initCafSS

	initV		= makeInitVar moduleName

	super		= [ PProto initV [] TObj
			  , PSuper initV [] TObj initSS ]

   in	super ++ cTree

makeInitCaf v
 = 	[ SHackery ("     _ddcCAF_" ++  name ++ " = " ++ "_ddcSlotPtr++;")
	, SHackery ("     _CAF(" ++ name ++ ") = 0;")
--	, SHackery ("     _CAF(" ++ name ++ ") = primSuspend0 ( (void*)" ++ name ++ ");\n") ] 
	, SHackery ("     _CAF(" ++ name ++ ") = " ++ name ++ "();\n") ] 

	where	name	= seaVar False v
		 
makeInitVar (ModuleAbsolute vs)
	= Var.new ("ddcInitModule_" ++ (catInt "_" vs))

-----
mainTree
	:: [Module]		-- Modules in this program
	-> Module		-- The module holding the Disciple level main function
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


