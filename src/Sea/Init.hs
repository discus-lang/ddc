
module Sea.Init
(
	initTree,
	mainTree,
	gotMain
)

where

import Util
import qualified Shared.Var	as Var
import Shared.Var		(Module(..))

import Sea.Exp
import Sea.Pretty

initTree 
	:: Module		-- name of this module
	-> Tree () 		-- source tree
	-> Tree ()

initTree moduleName cTree
 = let 	initAtomSS	= [ SAssign (XAtom v) t $ XAllocDataAnchored v 0
				| PAtom v t	<- cTree]

	initCafSS	= catMap makeInitCaf [ v | PCafSlot v <- cTree]
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
	:: [Module]
	-> Tree ()
	
mainTree imports
 = let
	sLine	
	 = unlines $
		[ "int main (int argc, char** argv)"
		, "{"
		, "        _ddcRuntimeInit (argc, argv);" 
		, "" ]

		-- call all the init functions for imported modules.
	 ++ 	map (\m -> "\t" ++ "_" ++ (Var.name $ makeInitVar m) ++ "();") imports

		-- call the init function for the main module.
	 ++	[ "        _ddcInitModule_Main();"]
		
		-- catch exceptions from the main function so we can display nice 
		--	error messages.
	 ++ 	[ ""
		, "        Control_Exception_topHandle(_allocThunk(Main_main, 1, 0));"
		, ""
		, "        _ddcRuntimeCleanup();"
		, "}"
		, ""
		]

   in	[PHackery sLine]


-----
gotMain	:: Tree () -> Bool
gotMain	   tree
 	= or 
	$ map (\p -> case p of
			PSuper v _ _ _
			 | Var.nameModule v == ModuleAbsolute ["Main"] 
			 , Var.name v 	== "main"	-> True
			_				-> False)
	$ tree

