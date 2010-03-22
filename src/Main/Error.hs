
module Main.Error where
import DDC.Main.Pretty


data Error
	= ErrorNoMainInMain
	| ErrorLinkExecutableWithoutMain
	| ErrorSymbolInFileName 	Char
	| ErrorRecursiveModules 	String
	| ErrorCantFindRuntime		[FilePath]
	| ErrorCantFindLibrary		[FilePath]
	| ErrorNotOverWritingDirectory	FilePath
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

 ppr (ErrorCantFindRuntime paths)
	= vcat
	[ ppr "Can't find the DDC runtime system.\n"
	, ppr "    Please supply a '-basedir' option to specify the directory\n"
	, ppr "    containing 'runtime/ddc-runtime.so'\n"
	, ppr "\n"
	, ppr "    tried:\n" %> vcat paths % "\n\n"
	, ppr "    use 'ddc -help' for more information\n"]

 ppr (ErrorCantFindLibrary paths)
	= vcat
	[ ppr "Can't find the DDC base library.\n"
	, ppr "    Please supply a '-basedir' option to specify the directory\n"
	, ppr "    containing 'library/Base.ds'\n"
	, ppr "\n"
	, "    tried:\n" %> vcat paths % "\n\n"
	, ppr "    use 'ddc -help' for more information\n"]
	
 ppr (ErrorNotOverWritingDirectory outFileName)
	= "A directory already exists with this name: " % outFileName

