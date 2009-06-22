
module Main.Error where

import Shared.Pretty

data Error
	= ErrorNoMainFunction
	deriving Show

instance Pretty Error PMode where
 ppr (ErrorNoMainFunction)
	= ppr $ unlines
	[ "    Main module does not define the 'main' function."]

