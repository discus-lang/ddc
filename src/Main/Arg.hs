
module Main.Arg
(
	Arg (..),
	parse,
	expand,
	helpString,
	options
)

where

import Util.List
import Util.Options
import Main.Version
import Main.Path

-----------------------
-- Arg
--	Data type to hold command line arguments.
--
data Arg
	= Error String

	-- general
	| Help
	| Verbose
	| Compile	[String]
	| InputFile	[String]
	| OutputFile	[String]
	| ImportDirs	[String]
	| Make		[String]

	| StopConstraint
	| StopType
	| StopCore
	| StopSea
	| StopCompile
	| StopErrors	[String]

	| KeepCFiles
	| KeepOFiles
	| NoImplicitPrelude
	| ArgPath	Path

	-- lint options
	| LintAll
	| LintCore
	
	-- type checker
	| NoEffects
	| NoInducedFetters
	| NoPurityCheck
	| GenDangerousVars
	
	-- code generation
	| Debug
	| StaticRuntime
	| Profile
	| ProfileRuntime
	
	-- linker
	| LinkObj	[String]
	| LinkLib	[String]
	| LinkLibDir	[String]
	
	-- dump
	| DumpAll

	| DumpSource

	| DumpSourceParsed
	| DumpSourceDefixed
	| DumpSourceRenamed
	| DumpSourceRenameTrace
	| DumpSourceKinds
	| DumpSourceAliased

	| DumpDesugared
	| DumpDesugaredProject
	| DumpDesugaredSlurped

	| DumpTypeSlurp
	| DumpTypeConstraints
	| DumpTypeSolve

	| DumpCoreSchemes
	| DumpCore
	| DumpCoreClean
	| DumpCoreBlock
	| DumpCoreSnip
	| DumpCoreCrush
	| DumpCoreDict
	| DumpCoreReconstruct
	| DumpCoreBind
	| DumpCoreMaskEffs
	| DumpCorePrim
	| DumpCoreBoxing
	| DumpCoreFullLaziness
	| DumpCoreInline
	| DumpCoreLifted
	| DumpCoreLabelIndex
	| DumpCoreSequence
	| DumpCoreCurry
	| DumpCoreAtomise
	| DumpCoreDitch

	| DumpSea
	| DumpSeaSub
	| DumpSeaCtor
	| DumpSeaThunk
	| DumpSeaForce
	| DumpSeaSlot
	| DumpSeaFlatten
	| DumpSeaInit

	-- opt
	| OptAll
	| OptAtomise
	| OptBoxing
	| OptFullLaziness
	| OptInline
	| OptTailCall
	
	-- graph

	| GraphModules
	| GraphModulesCut	[String]

	| GraphApps
	| GraphCalls
	| GraphSuperDeps

	| GraphTypeGraph
	| GraphTypeVars		[String]

	-- junk
	| AnnotAll
	| AnnotMisc
	| AnnotVar
	| AnnotType

	deriving (Show, Eq, Ord)

-----
-- expand
--	Turns a list of freshly parsed command arguments into a more useful form.
--
expand ::	[Arg] 	-> [Arg]
expand		[]	= []
expand		(x:xs)
 = case x of
	AnnotAll	
	 -> x : 
	   [AnnotMisc, AnnotVar, AnnotType] 
	 ++ expand xs

	Compile{}
	 -> x : StopCompile : KeepOFiles : expand xs

	Make ss
	 -> x : Compile ss : expand xs

	StopSea
	 -> x : KeepCFiles : expand xs
	 
	StopCompile
	 -> x : KeepOFiles : expand xs

	OptAll
	 -> 	[ -- OptAtomise
	 	  OptBoxing 
		, OptTailCall ] 
		-- OptFullLaziness ]
	 ++ expand xs

	DumpAll		
	 -> x : 
	   [DumpSource
	   , DumpSourceRenameTrace
	   , DumpTypeConstraints
	   , DumpTypeSlurp
	   , DumpTypeSolve
	   , DumpCore
	   , DumpCoreLifted
	   , DumpCoreCurry
	   , DumpSea ]
	 ++ expand xs
	 
	LintAll
	 -> x : [LintCore]
	  ++ expand xs
	 
	GraphTypeVars{}
	 -> x : GraphTypeGraph : expand xs
	_		
	 -> x : expand xs


parse ::	String -> [Arg]
parse		ss
 = let
 	(errs, args)	= munch options $ tokenise ss
	args'		= expand args
	

  in 	(map Error errs) ++ (nub args')
	
	
helpString
	= unlines $
	[ "Trauma " ++ version
	, "  usage: trauma [options] files.." ]
	++ [makeOptionHelp 40 options]



-----------------------
-- Option table
--	All option tokens must start with a '-'
--
options	= 
	
	[ ODefault	InputFile

	-- general
	, OGroup	"general"	
			"General Options."
			
	, OFlag		Help			
			["-h", "-help", "--help"]
			"Display this help."

	, OFlag 	Verbose	
			["-v", "-verbose"]
			"Print debugging info to stdout."

	, OOpts		Compile
			["-c",	"-compile"]
			"-c, -compile <files..>"
			"Compile .ts files to .o files. Implies -stop-compile, -keep-o."

	, OOpts		ImportDirs
			["-i", "-import"]
			"-i, -import  <dirs..>"
			"Add dirs to the import path."

	, OOpts		Make
			["-m", "-make"]
			"-m, -make    <files..>"
			"Compile and link objects."

	, OOpts		OutputFile
			["-o", "-output"]
			"-o, -output  <files..>"
			"Output files."
	, OBlank
	, OFlag		StopConstraint
			["-stop-constraint"]
			"Stop after generating type constraints."

	, OFlag		StopType
			["-stop-type"]
			"Stop after type checking program."

	, OFlag		StopCore
			["-stop-core"]
			"Stop after finishing core transforms."

	, OFlag		StopSea
			["-stop-c"]
			"Stop after producing .c files, do not invoke gcc. Implies -keep-c."
		 
	, OFlag		StopCompile
			["-stop-compile"]
			"Stop after producing .o files, do not invoke linker. Implies -keep-o."

	, OOpts		StopErrors
			["-stop-errors"]
			"-stop-errors <file>"
			"If there are errors in the source then write them to <file> and exit successfully."

	, OBlank
	, OFlag		KeepCFiles
			["-keep-c"]
			"Keep intermediate .c and .h files."

	, OFlag 	KeepOFiles
			["-keep-o"]
			"Keep intermediate .o files."

	, OBlank
	, OFlag		NoImplicitPrelude
			["-no-implicit-prelude"]
			"Don't implicitly import Prelude."

	-- code generation
	, OGroup	"code"
			"Code Generation."
			
	, OFlag		Debug			["-debug"]			""
	, OFlag		StaticRuntime		["-static-runtime"]		"Statically link the runtime system."
	, OFlag		Profile			["-profile"]			""
	, OFlag		ProfileRuntime		["-profile-runtime"]		""

	-- linker
	, OGroup	"link"
			"Linker"
			
	, OOpts 	LinkObj
			["-link-obj"]
			"-link-obj <files..>"
			"Also link with these objects."

	, OOpts		LinkLib
			["-l", "-link-lib"]
			"-l, -link-lib <libs..>"
			"Also link against these libraries."

	, OOpts		LinkLibDir
			["-L", "-link-lib-dir"]
			"-L, -link-lib-dir <dirs..>"
			"Also search for libraries in these directories."
			
	-- opt
	, OGroup	"opt"			
			"Optimisation."
			
	, OFlag		OptAll			["-O"]				"Perform all optimizations."
	, OFlag		OptAtomise		["-opt-atomise"]		"Share constructors of zero arity."
	, OFlag		OptBoxing		["-opt-boxing"]			"Locally unbox data with basic types."
	, OFlag		OptFullLaziness		["-opt-full-laziness"]		"Perform full laziness optimization."
	, OFlag		OptTailCall		["-opt-tail-call"]		"Perform tail call optimisation."
	, OFlag		OptInline		["-opt-inline"]			"Perform inlining."

	-- lint
	, OGroup	"lint"
			"Internal consistency checks."
			
	, OFlag		LintAll			["-lint"]			"Perform all available lint checks."
	, OFlag		LintCore		["-lint-core"]			"Check for lint in Core IR."
	
	-- type checker
	, OGroup	"type"
			"Type Inference."
			
	, OFlag		NoEffects		["-no-effects"]			"(unsafe) Do not infer effect information."
	, OFlag		NoInducedFetters	["-no-induced-fetters"]		"(unsafe) Do not infer induced fetters."
	, OFlag		NoPurityCheck		["-no-purity-check"]		"(unsafe) Do not check purity constraints."
	, OFlag		GenDangerousVars	["-gen-dangerous-vars"]		"(unsafe) Generalise dangerous type variables."

	-- dump
	, OGroup	"dump"
			"Dumping/Tracing."
			
	, OFlag 	DumpAll			["-dump"]			"Dump everything."

	, OFlag		DumpSource		["-dump-source"]		""
	, OFlag		DumpSourceParsed	["-dump-source-parsed"]		"Parsed source file."
	, OFlag		DumpSourceDefixed	["-dump-source-defixed"]	"Infix applications converted to prefix."
	, OFlag		DumpSourceRenamed	["-dump-source-renamed"]	"Unique identifiers introduced for variables."
	, OFlag 	DumpSourceRenameTrace	["-dump-source-rename-trace"]	""
	, OFlag		DumpSourceKinds		["-dump-source-kinds"]		""
	, OFlag		DumpSourceAliased	["-dump-source-aliased"]	""
	, OBlank

	, OFlag		DumpDesugared		["-dump-desugared"]		"Desugared IR version of Source code."
	, OFlag		DumpDesugaredProject	["-dump-desugared-project"]	"Add default projections and snip dictionaries."
	, OFlag		DumpDesugaredSlurped	["-dump-desugared-slurped"]	"Slurp type constraints and add type vars to code."
	, OBlank

	, OFlag		DumpTypeConstraints	["-dump-type-constraints"]	"Type constraints from Desugared IR."
	, OFlag		DumpTypeSlurp		["-dump-type-slurp"]		"Dump type slurper debugging info."
	, OFlag		DumpTypeSolve		["-dump-type-solve"]		"Dump type solver debugging info."

	, OBlank

	, OFlag		DumpCoreSchemes		["-dump-core-schemes"]		"Inferred type schemes converted to Core IR."
	, OFlag		DumpCore		["-dump-core"]			"Core IR version of Desugared IR code."
	, OFlag		DumpCoreClean		["-dump-core-clean"]		"Clean out empty effect/closure variables."
	, OFlag		DumpCoreBlock		["-dump-core-block"]		"Applications placed in Do blocks."
	, OFlag		DumpCoreSnip		["-dump-core-snip"]		"Fresh bindings for all function applications."
	, OFlag		DumpCoreCrush		["-dump-core-crush"]		"Do-Do expressions crushed to single Dos."
	, OFlag		DumpCoreDict		["-dump-core-dict"]		"Call class instance functions and add dictionaries."
	, OFlag		DumpCoreReconstruct	["-dump-core-reconstruct"]	"Type information reconstructed for all bindings."
	, OFlag		DumpCoreBind		["-dump-core-bind"]		"All regions bound and annotated with fetters."
	, OFlag		DumpCoreMaskEffs	["-dump-core-mask-effs"]	"Mask out effects on Const regions."
	, OFlag		DumpCorePrim		["-dump-core-prim"]		"Identify primitive operations."
	, OFlag		DumpCoreBoxing		["-dump-core-boxing"]		"Local unboxing optimisations."
	, OFlag		DumpCoreFullLaziness	["-dump-core-full-laziness"]	"Full laziness optimisation."
	, OFlag		DumpCoreInline		["-dump-core-inline"]		"Inlining."
	, OFlag		DumpCoreLifted		["-dump-core-lifted"]		"Lambda lifting."
	, OFlag		DumpCoreLabelIndex	["-dump-core-labelIndex"]	"Convert field labels to indicies."
	, OFlag		DumpCoreSequence	["-dump-core-sequence"]		"Sequenced CAFs and bindings."
	, OFlag		DumpCoreCurry		["-dump-core-curry"]		"Detect currying vs direct super calls."
	, OFlag		DumpCoreAtomise		["-dump-core-atomise"]		"Share instances of zero airity data objects."
	, OFlag		DumpCoreDitch		["-dump-core-ditch"]		"Erase extraneous type info for Sea conversion."
	, OBlank

	, OFlag		DumpSea			["-dump-sea"]			"Sea IR version of Core IR."
	, OFlag		DumpSeaSub		["-dump-sea-sub"]		"Substitute trivial v1 = v2 bindings."
	, OFlag		DumpSeaCtor		["-dump-sea-ctor"]		"Expand code for constructor applications."
	, OFlag		DumpSeaThunk		["-dump-sea-thunk"]		"Expand code for thunk building."
	, OFlag		DumpSeaForce		["-dump-sea-force"]		"Force lazy objects."
	, OFlag		DumpSeaSlot		["-dump-sea-slot"]		"Store intermediate object ptrs on GC slot stack."
	, OFlag		DumpSeaFlatten		["-dump-sea-flatten"]		"Flatten out match statements."
	, OFlag		DumpSeaInit		["-dump-sea-init"]		"Emit module initialisation code."
	, OBlank

	-- graphing
	, OGroup	"graph"			"Graphing."
	, OFlag		GraphModules		["-graph-modules"]		"Module hierarchy graph."

	, OOpts		GraphModulesCut
			["-graph-modules-cut"]
			"-graph-modules-cut <modules..>"
			"Don't include these modules or their imports in the module hierarchy graph."

	, OFlag		GraphApps		["-graph-apps"]			"Function application graph."
	, OFlag		GraphCalls		["-graph-calls"]		"Function call graph."
	, OFlag		GraphSuperDeps		["-graph-super-deps"]		"Supercombinator and CAF dependency graph."
	
	, OFlag		GraphTypeGraph		["-graph-type-graph"]		""
	, OOpts		GraphTypeVars		["-graph-type-vars"]		"-graph-type-vars <names..>"	""

	]
				








