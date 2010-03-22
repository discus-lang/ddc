
-- | DDC command line arguments.
module DDC.Main.Arg
	( Arg	(..)
	, expandArgs
	, takePrettyModeOfArg )
where
import DDC.Main.Pretty


-- | DDC command line arguments.
data Arg
	= Error String			-- ^ represents some failure of options parsing
					--	Used in Main.ParseArgs

	-- general args
	| Help		[String]	-- ^ dump help screen.
	| Verbose			-- ^ verbose compilation.
	| Quiet				-- ^ quiet compilation.
	| Compile	[String]	-- ^ compile just these files, no link.
	| Build		[String]	-- ^ recursive build.
	| Make		[String]	-- ^ recursive build then link executable.
	| InputFile	String		-- ^ set input file.
	| OutputFile	String		-- ^ set output file.
	| ImportDirs	[String]	-- ^ dirs to look for imports.
	| PathBase	String		-- ^ base path to libraries.
	| NoImplicitPrelude		-- ^ don't implicitly import the prelude.

	-- lint options
	| LintAll			-- ^ run all lint passes.
	| LintCore			-- ^ lint the core program.

	-- type system options
	| GenDangerousVars		-- ^ generalise dangrous type variables.

	-- optimisation
	| OptAll			-- ^ perform all optimistaions.
	| OptSimplify			-- ^ do core simplification
	| OptTailCall			-- ^ do tail-call optimisation

	-- code generation
	| Debug				-- ^ compile generated C files with GNU debugging
	| StaticRuntime			-- ^ link against static runtime
	| Profile			-- ^ profile generated C files
	| ProfileRuntime		-- ^ link against profiled runtime.
	
	-- linker	
	| Link				-- ^ perform linking
	| LinkObj	[String]	-- ^ extra object to link with
	| LinkLib	[String]	-- ^ extra library to link with
	| LinkLibDir	[String]	-- ^ dir to search for extra link libraries

	-- partial compilation
	| StopConstraint		-- ^ stop after generating type constraints
	| StopType			-- ^ stop after solving type constraints
	| StopCore			-- ^ stop after producing core program
	| StopSea			-- ^ stop after producing C program
	| StopCompile			-- ^ stop after compiling C program, no link

	| StopErrors   [FilePath]	-- ^ on error, dump error messages to the given
					--   file names and succeed.

	| KeepCFiles			-- ^ keep intermediate .c files
	| KeepOFiles			-- ^ keep intermediate .o files


	-- Dump flags ---------------------------------------------------------
	-- NOTE: If you add something here then make sure it's also present
	--       in the expansion for DumpAll in the expandArgs function below.
	--
	
	-- dump pretty printing options 
	| DumpPrettyUnique	
	| DumpPrettyTypeSpaces
	| DumpPrettyTypeKinds
	| DumpPrettyCoreTypes
	| DumpPrettyCoreMore

	-- dump everything
	| DumpAll

	-- source dumps
	| DumpSourceTokens
	| DumpSourceParse
	| DumpSourceDefix
	| DumpSourceRename

	-- desugar dumps
	| DumpDesugar
	| DumpDesugarKinds
	| DumpDesugarElaborate
	| DumpDesugarProject
	| DumpDesugarSlurp

	-- type dumps
	| DumpTypeConstraints
	| DumpTypeSolve

	-- core dumps
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
	| DumpCoreLift
	| DumpCoreLabelIndex
	| DumpCoreSequence
	| DumpCoreCurry

	-- sea dumps
	| DumpSea
	| DumpSeaSub
	| DumpSeaCtor
	| DumpSeaThunk
	| DumpSeaForce
	| DumpSeaSlot
	| DumpSeaFlatten
	| DumpSeaInit
	deriving (Show, Eq, Ord)


-- | Expand out arguments that are implied by others
expandArgs :: [Arg]  -> [Arg]
expandArgs []	= []
expandArgs (x:xs)
 = case x of

	-- general args
	Compile{}
	 -> x : StopCompile : KeepOFiles : expandArgs xs

	Make ss
	 -> x : expandArgs xs

	LintAll
	 -> x : [LintCore]
	  ++ expandArgs xs

	-- optimisations
	OptAll
	 -> 	[ OptAll
	 	, OptSimplify
		, OptTailCall ]
	 ++ expandArgs xs

	-- stopping after stages
	StopSea
	 -> x : KeepCFiles : expandArgs xs
	 
	StopCompile
	 -> x : KeepOFiles : expandArgs xs

	-- dump flags
	DumpAll		
	 -> x : 
	   [ DumpSourceTokens
	   , DumpSourceParse
	   , DumpSourceDefix
	   , DumpSourceRename

	   , DumpDesugar
	   , DumpDesugarKinds
	   , DumpDesugarElaborate
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

	 ++ expandArgs xs
	 
	_		
	 -> x : expandArgs xs


-- | Convert an arg into the pretty mode it enables
takePrettyModeOfArg :: Arg -> Maybe PrettyMode
takePrettyModeOfArg aa
 = case aa of
 	DumpPrettyUnique	-> Just $ PrettyUnique
	DumpPrettyTypeSpaces	-> Just $ PrettyTypeSpaces
	DumpPrettyTypeKinds	-> Just $ PrettyTypeKinds
	DumpPrettyCoreTypes	-> Just $ PrettyCoreTypes
	DumpPrettyCoreMore	-> Just $ PrettyCoreMore
	_			-> Nothing
