
module Main.Error where

import Shared.Pretty

data Error
	= ErrorNoMainFunction
	| ErrorSymbolInFileName Char
	deriving Show

instance Pretty Error PMode where
 ppr (ErrorNoMainFunction)
	= ppr $ unlines
	[ "    Main module does not define the 'main' function."]

 ppr (ErrorSymbolInFileName sym)
	= ppr $ unlines
	[ "    Invalid symbol " ++ show sym ++ " in source file name."]



