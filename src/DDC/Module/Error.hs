
module DDC.Module.Error where
import DDC.Main.Pretty

-- | Framework errors that can arise when compiling the module.
--   Keep problems with the actual source code in on of the other error types
--   like in "DDC.Source.Error" or "DDC.Solve.Error".
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
	= indent
	$ " Main module does not define the 'main' function."

 ppr ErrorLinkExecutableWithoutMain
	= indent
	$ "Can't create executable as the 'main' function is not defined."

 ppr (ErrorSymbolInFileName sym)
	= indent 
	$ "Invalid symbol" %% show sym %% "in source file name."

 ppr (ErrorRecursiveModules str)
	= indent str

 ppr (ErrorCantFindRuntime paths)
	= indent
	$  "Can't find the DDC runtime system."
	%! "    Please supply a '-basedir' option to specify the directory"
	%! "    containing 'runtime/ddc-runtime.so'"
 	%! blank
 	%! "    tried:" % nl %> vcat paths

 ppr (ErrorCantFindLibrary paths)
	= indent
 	$  "Can't find the DDC base library.\n"
	%! "    Please supply a '-basedir' option to specify the directory\n"
	%! "    containing 'library/Base.ds'\n"
	%! blank
	%! "    tried:" % nl %> vcat paths
	
 ppr (ErrorNotOverWritingDirectory outFileName)
	= "A directory already exists with this name: " % outFileName

