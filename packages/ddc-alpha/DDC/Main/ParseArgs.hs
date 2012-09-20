
-- | Command line argument parsing.
module DDC.Main.ParseArgs
	( parse
	, helpString
	, options)
where
import DDC.Main.Arg
import DDC.Main.Pretty
import DDC.Config.Version
import Util.Options
import Util.Options.Option
import Util.Options.Help
import Util


-- | Parse some command line arguments.
parse :: [String] -> [Arg]
parse strArgs
 = let	(errs, args)	= parseOptions options strArgs
	args'		= expandArgs args
  in 	(map Error errs) ++ (nub args')
	

-- | Build the DDC option help page
helpString :: [Arg] -> String	
helpString args
 = let	
 	-- work out sections we've been asked for
	secs	= concat [ss | Help ss <- args]
	
	-- if no sections were requested then display the general ones
	secs_display
		= case secs of
			[]	-> ["general"]
			_	-> secs

   in	helpString2 args secs secs_display
   
helpString2 args secs secs_display

	-- check if any unknown manual sections were asked for
	| badSecs	<- filter (\s -> not $ elem s ("all" : "contents" : sections)) secs
	, not $ null badSecs
	= pprStrPlain
		$ "  Unknown help section(s): " % " " %!% badSecs % "\n"
		% "   Available sections are: " % " " %!% sections % "\n\n"
	
	| otherwise
	= let
		-- If no specific sections were asked for then add the meta-help as well
		metaHelp
		 | secs == []
		 = unlines
		 	[ ""
			, "  Meta help:"
			, "    ddc -help all                  Display help for all options."
			, "    ddc -help contents             Show the names of help pages."
			, "    ddc -help <sections..>         Show these particular help pages." ]

		 | otherwise
		 = []
	 
		-- build the help string
	  	help	=  ddcName ++ "." ++ "\n"
			++ "  Usage: ddc <options..>\n" 
			++ metaHelp
			++ makeOptionHelp 35 secs_display options
			++ "\n"
	   in	help


-- | Section table
--	These are the available option sections.
sections = [s	| OGroup s _ <- options]


-- | Option table
--	All options must start with a '-'
options	= 
	
	[ ODefault	InputFile

	-- General options, displayed by defailt.
	, OGroup	"general"		"General Options."
	, OOpts		Help			["-h", "-help", "--help"]	"-h, -help <sections..>"
						"Print help on DDC options"	
			
	, OFlag 	Verbose			["-v", "-verbose"]		"Print debugging info to stdout."
	, OFlag		Quiet			["-q", "-quiet"]		"Don't print build info to stdout."

	, OOpt		PathBase		["-B",  "-basedir"]		"-B, -basedir <path>"
						"The base directory containing ./library and ./runtime"
	, OBlank
	, OOpts		Compile			["-c",	"-compile", "--compile"] "-c, -compile <files..>"
						"Compile .ds to .o"

	, OOpts		Build			["-b", "-build", "--build"]	"-b, -build <files.>"
						"Recursively compile these source files."

	, OOpts		Make			["-m", "-make", "--make"]	"-m, -make    <files..>"
						"Recursively compile and link these sources into an executable"

	, OOpt		OutputFile		["-o", "-output"]		"-o, -output  <file>"
						"Redirect output to this file."

	, OOpt		OutputDir		["-outputdir"]			"-outputdir <dir>"
						"Redirect output files to this dir."

	, OOpts		ImportDirs		["-i", "-import"]		"-i, -import  <dirs..>"
						"Add dirs to the import path."
	, OBlank
	, OFlag		ViaLLVM			["-fvia-llvm"]			"Compile via LLVM."
	, OBlank
	, OFlag		NoImplicitPrelude	["-no-implicit-prelude"]	
						"Don't implicitly import Prelude."

	, OFlag		NoImplicitHandler	["-no-implicit-handler"]
						"Don't implicitly use the Prelude's top-level exception handler."

	, OBlank
	, OFlag		LintAll			["-lint"]			"Perform all available lint checks. (default)"
	, OFlag		LintCore		["-lint-core"]			"Check for lint in Core IR. (default)"


	-- Optimisation.
	, OGroup	"opt"			"Optimisation."
	, OFlag		OptAll			["-O"]				"Perform all optimizations."
	, OFlag		OptSimplify		["-opt-simplify"]		"Do core simplification."
	, OFlag		OptTailCall		["-opt-tail-call"]		"Perform tail call optimisation. (default)"

	-- Code Generation.
	, OGroup	"code"			"Code Generation."
	, OFlag		Debug			["-debug"]			"Add debugging symbols to object file. (for gdb)"
	, OFlag		Profile			["-profile"]			"Profile the object file (for gprof)."

	-- Linking.
	, OGroup	"link"			"Linker."
	, OFlag		Link			["-link"]			"Produce an executable"

	, OOpts		LinkLib			["-l", "-link-lib"]		"-l, -link-lib <libs..>" 
										"Also link against these libraries."

	, OOpts		LinkLibDir		["-L", "-link-lib-dir"]		"-L, -link-lib-dir <dirs..>"
										"Also search for libraries in these directories."
			
	, OOpts 	LinkObj			["-link-obj"]			"-link-obj <files..>"
										"Also link with these objects."
	, OBlank
	, OFlag		StaticRuntime		["-static-runtime"]		"Statically link the runtime system."
			
	-- Stopping.
	, OGroup	"partial"		"Partial Compilation"
	, OFlag		StopConstraint		["-stop-constraint"]		"Stop after generating type constraints."
	, OFlag		StopType		["-stop-type"]			"Stop after type checking program"
	, OFlag		StopCore		["-stop-core"]			"Stop after finishing core transforms."
	, OFlag		StopSea			["-stop-c"]			"Stop after producing .c files. Implies -keep-c."
	, OFlag		StopCompile		["-stop-compile"]		"Stop after producing .o files. Implies -keep-o."

	, OOpts		StopErrors		["-stop-errors"]		"-stop-errors <file>"
										"Write errors to <file> and exit successfully."	
	, OBlank
	, OFlag		KeepCFiles		["-keep-c"]			"Keep intermediate .c and .h files."
	, OFlag 	KeepOFiles		["-keep-o"]			"Keep intermediate .o files."

	-- Compiler debugging.
	-- These options have long names, so the descriptions have extra
	-- spaces at the start so the help page comes out formatted ok.
	, OGroup	"debug"
			"Hacks for debugging the compiler"

	, OFlag		DebugGeneraliseDangerousVars
			["-debug-generalise-dangerous-vars"]		
			"  Generalise dangerous type variables (makes type system unsound)."

	, OFlag		DebugNoConstraintSimplifier
			["-debug-no-constraint-simplifier"]	
			"   Don't run the constraint simplifer before solving (should always be ok)."

	-- Dumping intermediate representations.
	, OGroup	"dump"			"Dumping/Tracing."
	, OFlag		DumpPrettyUnique	["-dump-pretty-unique"]		"Append unique identifiers to variables."
	, OFlag		DumpPrettyTypeSpaces	["-dump-pretty-type-spaces"]	"Show a '*' namespace qualifier on type vars."
	, OFlag		DumpPrettyTypeKinds	["-dump-pretty-type-kinds"]	"Show kinds on type vars and constructors."
	, OFlag		DumpPrettyCoreTypes	["-dump-pretty-core-types"]	"Show type annots on vars in core."
	, OFlag		DumpPrettyCoreMore	["-dump-pretty-core-more"]	"Show :> constraints on type vars in core."
	, OFlag		DumpPrettySeaTypes	["-dump-pretty-sea-types"]	"Show type annots on vars in the sea program.."
	, OBlank
	, OFlag 	DumpAll			["-dump"]			"Dump everything."
	, OBlank
	, OFlag		DumpNewInterfaces	["-dump-new-interfaces"]	"Dump new-format interface file."
	, OBlank
	, OFlag		DumpSourceTokens	["-dump-source-tokens"]		"Tokenised source file."
	, OFlag		DumpSourceParse		["-dump-source-parse"]		"Parsed source file."
	, OFlag		DumpSourceRename	["-dump-source-rename"]		"Unique identifiers introduced for variables."
	, OFlag		DumpSourceDefix		["-dump-source-defix"]		"Infix applications converted to prefix."
	, OBlank
	, OFlag		DumpDesugar		["-dump-desugar"]		"Desugared version of source code."
	, OFlag		DumpDesugarKinds	["-dump-desugar-kinds"]		"Infer kinds and annotate types."
	, OFlag		DumpDesugarElaborate	["-dump-desugar-elaborate"]	"Elaborate types and effects in sigs."
	, OFlag		DumpDesugarProject	["-dump-desugar-project"]	"Add default projections and snip dictionaries."
	, OFlag		DumpDesugarSlurp	["-dump-desugar-slurp"]		"Slurp type constraints and add type vars to code."
	, OBlank
	, OFlag		DumpTypeConstraints	["-dump-type-constraints"]	"Type constraints from desugared code."
	, OFlag		DumpTypeSolve		["-dump-type-solve"]		"Trace of constraint solver."
	, OBlank
	, OFlag		DumpCore		["-dump-core"]			"Core version of desugared code."
	, OFlag		DumpCoreTidy		["-dump-core-tidy"]		"Tidy up after conversion to core."
	, OFlag		DumpCoreBind		["-dump-core-bind"]		"Bind regions locally within functions."
	, OFlag		DumpCoreSnip		["-dump-core-snip"]		"Create fresh bindings for function applications."
	, OFlag		DumpCoreThread		["-dump-core-thread"]		"Thread witnesses through bindings."
	, OFlag		DumpCoreLint		["-dump-core-lint"]		"Check type information."
	, OFlag		DumpCoreDict		["-dump-core-dict"]		"Resolve type-class overloading of functions."
	, OFlag		DumpCorePrim		["-dump-core-prim"]		"Identify primitive operations."
	, OFlag		DumpCoreSimplify	["-dump-core-simplify"]		"Core simplification. (when enabled)"
	, OFlag		DumpCoreLift		["-dump-core-lift"]		"Convert nested functions to supercombinators."
	, OFlag		DumpCoreCurry		["-dump-core-curry"]		"Identify super calls vs curried applications."
	, OFlag		DumpCorePrep		["-dump-core-prep"]		"Prepare for conversion to Sea."
	, OBlank
	, OFlag		DumpSea			["-dump-sea"]			"Sea IR version of Core IR."
	, OFlag		DumpSeaSub		["-dump-sea-sub"]		"Substitute trivial v1 = v2 bindings."
	, OFlag		DumpSeaCtor		["-dump-sea-ctor"]		"Expand code for constructor applications."
	, OFlag		DumpSeaThunk		["-dump-sea-thunk"]		"Expand code for thunk building."
	, OFlag		DumpSeaForce		["-dump-sea-force"]		"Rewrite switch stmts to force lazy objects."
	, OFlag		DumpSeaSlot		["-dump-sea-slot"]		"Store intermediate object ptrs on GC slot stack."
	, OFlag		DumpSeaFlatten		["-dump-sea-flatten"]		"Flatten out match statements."
	, OFlag		DumpSeaInit		["-dump-sea-init"]		"Insert module initialisation code."
	, OBlank
	]
