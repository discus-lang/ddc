
module Main.Error where
import Shared.Pretty

data Error
	= ErrorNoMainInMain
	| ErrorLinkExecutableWithoutMain
	| ErrorSymbolInFileName Char
	| ErrorRecursiveModules String
	deriving Show

instance Pretty Error PMode where
 ppr (ErrorNoMainInMain)
	= ppr $ unlines
	[ "    Main module does not define the 'main' function."]

 ppr ErrorLinkExecutableWithoutMain
	= ppr $ unlines
	[ "    Can't create executable as the 'main' function is not defined."]

 ppr (ErrorSymbolInFileName sym)
	= ppr $ unlines
	[ "    Invalid symbol " ++ show sym ++ " in source file name."]

 ppr (ErrorRecursiveModules s)
	= ppr $ unlines
	[ "    " ++ s]


