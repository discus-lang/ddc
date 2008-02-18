
module Main.Arg
	( Arg (..)
	, parse
	, expand
	, helpString
	, options)

where

import Util.List
import Util.Options
import Util.Pretty
import Main.Version
import Main.Path

-- | Holds command line arguments
data Arg
	= Error String

	-- used internally
	| ArgPath	Path

	-- general
	| Help		[String]
	| Verbose
	| Compile	[String]
	| InputFile	[String]
	| OutputFile	[String]
	| ImportDirs	[String]
	| Make		[String]
	| PathBase	[String]

	| NoImplicitPrelude

	-- lint options
	| LintAll
	| LintCore

	-- type system options
	| GenDangerousVars

	-- optimisation
	| OptAll
	| OptAtomise
	| OptSimplify
	| OptFullLaziness
	| OptInline
	| OptTailCall

	-- code generation
	| Debug
	| StaticRuntime
	| Profile
	| ProfileRuntime
	
	-- linker
	| LinkObj	[String]
	| LinkLib	[String]
	| LinkLibDir	[String]

	-- graph
	| GraphModules
	| GraphModulesCut	[String]

	| GraphApps
	| GraphCalls
	| GraphSuperDeps

	-- partial compilation
	| StopConstraint
	| StopType
	| StopCore
	| StopSea
	| StopCompile
	| StopErrors	[String]

	| KeepCFiles
	| KeepOFiles

	-- dump
	| DumpAll

	| DumpSourceParse
	| DumpSourceDefix
	| DumpSourceRename

	| DumpDesugar
	| DumpDesugarProject
	| DumpDesugarSlurp

	| DumpTypeConstraints
	| DumpTypeSolve

	| DumpCore
	| DumpCoreBlock
	| DumpCoreCrush
	| DumpCoreSnip
	| DumpCoreClean
	| DumpCoreThread
	| DumpCoreRecon
	| DumpCoreDict
	| DumpCoreBind
	| DumpCorePrim
	| DumpCoreSimplify
	| DumpCoreFullLaziness
	| DumpCoreInline
	| DumpCoreLift
	| DumpCoreLabelIndex
	| DumpCoreSequence
	| DumpCoreCurry
	| DumpCoreAtomise

	| DumpSea
	| DumpSeaSub
	| DumpSeaCtor
	| DumpSeaThunk
	| DumpSeaForce
	| DumpSeaSlot
	| DumpSeaFlatten
	| DumpSeaInit


	deriving (Show, Eq, Ord)


-- | Expand out all options implied by these ones.
expand ::	[Arg] 	-> [Arg]
expand		[]	= []
expand		(x:xs)
 = case x of

	-- general
	Compile{}
	 -> x : StopCompile : KeepOFiles : expand xs

	Make ss
	 -> x : Compile ss : expand xs

	LintAll
	 -> x : [LintCore]
	  ++ expand xs

	-- optimisations
	OptAll
	 -> 	[ OptAll
	 	, OptSimplify
		, OptTailCall ]
	 ++ expand xs

	-- partial
	StopSea
	 -> x : KeepCFiles : expand xs
	 
	StopCompile
	 -> x : KeepOFiles : expand xs


	DumpAll		
	 -> x : 
	   [ DumpSourceParse
	   , DumpSourceDefix
	   , DumpSourceRename

	   , DumpDesugar
	   , DumpDesugarProject
	   , DumpDesugarSlurp

	   , DumpTypeConstraints
	   , DumpTypeSolve

	   , DumpCore
	   , DumpCoreBlock
	   , DumpCoreCrush
	   , DumpCoreSnip
	   , DumpCoreClean
	   , DumpCoreThread
	   , DumpCoreRecon
	   , DumpCoreDict
	   , DumpCoreBind
	   , DumpCorePrim
	   , DumpCoreSimplify
	   , DumpCoreLift
	   , DumpCoreLabelIndex
	   , DumpCoreSequence
	   , DumpCoreCurry

	   , DumpSea 
	   , DumpSeaSub
	   , DumpSeaCtor
	   , DumpSeaThunk
	   , DumpSeaForce
	   , DumpSeaSlot
	   , DumpSeaFlatten
	   , DumpSeaInit]

	 ++ expand xs
	 
	_		
	 -> x : expand xs


-- | Parse these arguments
parse ::	String -> [Arg]
parse		ss
 = let
 	(errs, args)	= munch options $ tokenise ss
	args'		= expand args
	

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
	= pprStr 
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
	  	help	=   ddcName ++ "." ++ "\n"
			++ "  Usage: ddc <options..>\n" 
			++ metaHelp
			++ makeOptionHelp 35 secs_display options
			++ "\n"
	   in	help

-- | Section table
--	These are the available option sections.
sections
	= [s	| OGroup s _ <- options]


-- | Option table
--	all options must start with a '-'
--
options	= 
	
	[ ODefault	InputFile

	-- general
	, OGroup	"general"	
			"General Options."
			
	, OOpts		Help
			["-h", "-help", "--help"]
			"-h, -help <sections..>"
			"Print help on DDC options"
			
	, OFlag 	Verbose	
			["-v", "-verbose"]
			"Print debugging info to stdout."

	, OOpts		PathBase
			["-B",  "-basedir"]
			"-B, -basedir <path>"
			"The base directory containing ./library and ./runtime"

	, OBlank
	, OOpts		Compile
			["-c",	"-compile"]
			"-c, -compile <files..>"
			"Compile .ts files to .o files."

	, OOpts		Make
			["-m", "-make"]
			"-m, -make    <files..>"
			"Compile and link objects."

	, OOpts		OutputFile
			["-o", "-output"]
			"-o, -output  <files..>"
			"Redirect output to these files."

	, OOpts		ImportDirs
			["-i", "-import"]
			"-i, -import  <dirs..>"
			"Add dirs to the import path."

	, OBlank
	, OFlag		NoImplicitPrelude
			["-no-implicit-prelude"]
			"Don't implicitly import Prelude."

	, OBlank
	, OFlag		LintAll			
			["-lint"]
			"Perform all available lint checks. (default)"

	, OFlag		LintCore
			["-lint-core"]
			"Check for lint in Core IR. (default)"

	-- type system
	, OGroup	"type"
			"Type System."

	, OFlag		GenDangerousVars	
			["-gen-dangerous-vars"]		
			"Generalise dangerous type variables. (danger)"

	-- optimisation
	, OGroup	"opt"			
			"Optimisation."
			
	, OFlag		OptAll			["-O"]				"Perform all optimizations."
	, OFlag		OptSimplify		["-opt-simplify"]		"Do core simplification."
	, OFlag		OptTailCall		["-opt-tail-call"]		"Perform tail call optimisation. (default)"
--	, OFlag		OptAtomise		["-opt-atomise"]		"Share constructors of zero arity."
--	, OFlag		OptFullLaziness		["-opt-full-laziness"]		"Perform full laziness optimization."
--	, OFlag		OptInline		["-opt-inline"]			"Perform inlining."

	-- code generation
	, OGroup	"code"
			"Code Generation."
			
	, OFlag		Debug			
			["-debug"]
			"Add debugging symbols to object file. (for gdb)"

	, OFlag		Profile
			["-profile"]
			"Profile the object file (for gprof)."


	-- linker
	, OGroup	"link"
			"Linker."

	, OOpts		LinkLib
			["-l", "-link-lib"]
			"-l, -link-lib <libs..>"
			"Also link against these libraries."

	, OOpts		LinkLibDir
			["-L", "-link-lib-dir"]
			"-L, -link-lib-dir <dirs..>"
			"Also search for libraries in these directories."
			
	, OOpts 	LinkObj
			["-link-obj"]
			"-link-obj <files..>"
			"Also link with these objects."

	, OBlank
	, OFlag		StaticRuntime		
			["-static-runtime"]
			"Statically link the runtime system."
			
	-- graphing
	, OGroup	"graph"
			"Graphing."

	, OFlag		GraphModules		
			["-graph-modules"]		
			"Produce a module hierarchy graph."

	, OOpts		GraphModulesCut
			["-graph-modules-cut"]
			"-graph-modules-cut <modules..>"
			"Don't include these modules or their imports."

	, OFlag		GraphSuperDeps		
			["-graph-super-deps"]		
			"Produce a supercombinator and CAF dependency graph."

	-- partial compilation
	, OGroup	"partial"
			"Partial Compilation"
	
	, OFlag		StopConstraint
			["-stop-constraint"]
			"Stop after generating type constraints."

	, OFlag		StopType
			["-stop-type"]
			"Stop after type checking program"

	, OFlag		StopCore
			["-stop-core"]
			"Stop after finishing core transforms."

	, OFlag		StopSea
			["-stop-c"]
			"Stop after producing .c files. Implies -keep-c."
		 
	, OFlag		StopCompile
			["-stop-compile"]
			"Stop after producing .o files. Implies -keep-o."

	, OOpts		StopErrors
			["-stop-errors"]
			"-stop-errors <file>"
			"Write errors to <file> and exit successfully."
			
	, OBlank
	, OFlag		KeepCFiles
			["-keep-c"]
			"Keep intermediate .c and .h files."

	, OFlag 	KeepOFiles
			["-keep-o"]
			"Keep intermediate .o files."

	-- dump
	, OGroup	"dump"
			"Dumping/Tracing."
	
	, OFlag 	DumpAll			["-dump"]			"Dump everything."
	, OBlank

	, OFlag		DumpSourceParse		["-dump-source-parse"]		"Parsed source file."
	, OFlag		DumpSourceDefix		["-dump-source-defix"]		"Infix applications converted to prefix."
	, OFlag		DumpSourceRename	["-dump-source-rename"]		"Unique identifiers introduced for variables."
	, OBlank

	, OFlag		DumpDesugar		["-dump-desugar"]		"Desugared version of source code."
	, OFlag		DumpDesugarProject	["-dump-desugar-project"]	"Add default projections and snip dictionaries."
	, OFlag		DumpDesugarSlurp	["-dump-desugar-slurp"]		"Slurp type constraints and add type vars to code."
	, OBlank

	, OFlag		DumpTypeConstraints	["-dump-type-constraints"]	"Type constraints from desugared code."
	, OFlag		DumpTypeSolve		["-dump-type-solve"]		"Trace of constraint solver."

	, OBlank

	, OFlag		DumpCore		["-dump-core"]			"Core version of desugared code."
	, OFlag		DumpCoreBlock		["-dump-core-block"]		"RHS of bindings made into do expressions."
	, OFlag		DumpCoreCrush		["-dump-core-crush"]		"Crush nested do expressions."
	, OFlag		DumpCoreBind		["-dump-core-bind"]		"Bind regions locally within functions."
	, OFlag		DumpCoreSnip		["-dump-core-snip"]		"Create fresh bindings for function applications."
	, OFlag		DumpCoreThread		["-dump-core-thread"]		"Thread witnesses through bindings."
	, OFlag		DumpCoreRecon		["-dump-core-recon"]		"Reconstruct type information."
	, OFlag		DumpCoreDict		["-dump-core-dict"]		"Resolve type-class overloading of functions."
	, OFlag		DumpCorePrim		["-dump-core-prim"]		"Identify primitive operations."
	, OFlag		DumpCoreSimplify	["-dump-core-simplify"]		"Core simplification. (when enabled)"
--	, OFlag		DumpCoreFullLaziness	["-dump-core-full-laziness"]	"Full laziness optimisation."
--	, OFlag		DumpCoreInline		["-dump-core-inline"]		"Inlining."
	, OFlag		DumpCoreLift		["-dump-core-lift"]		"Convert nested functions to supercombinators."
	, OFlag		DumpCoreLabelIndex	["-dump-core-labelIndex"]	"Convert field labels to indicies."
	, OFlag		DumpCoreSequence	["-dump-core-sequence"]		"Sequence CAFs and bindings into dependency order."
	, OFlag		DumpCoreCurry		["-dump-core-curry"]		"Identify super calls vs curried applications."
--	, OFlag		DumpCoreAtomise		["-dump-core-atomise"]		"Share instances of zero airity data objects."
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
