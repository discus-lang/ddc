{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | DDC command line arguments.
module DDC.Main.Arg
	( Arg	(..)
	, expandArgs
	, takePrettyModeOfArg )
where
import DDC.Main.Pretty


-- | DDC command line arguments.
data Arg
	= Error String			-- ^ Represents some failure of options parsing.
					--	Used in Main.ParseArgs.

	-- general args
	| Help		[String]	-- ^ Print help screen.
	| Verbose			-- ^ Verbose compilation.
	| Quiet				-- ^ Quiet compilation.
	| Compile	[String]	-- ^ Compile just these files, no link.
	| Build		[String]	-- ^ Recursive build.
	| Make		[String]	-- ^ Recursive build then link executable.
	| InputFile	String		-- ^ Set input file.
	| OutputFile	String		-- ^ Set output file.
	| ImportDirs	[String]	-- ^ Dirs to look for imports.
	| PathBase	String		-- ^ Base path to libraries.
	| NoImplicitPrelude		-- ^ Don't implicitly import the prelude.
	| NoImplicitHandler		-- ^ Don't use the prelude's top-level exception handler.

	-- lint options
	| LintAll			-- ^ Run all lint passes.
	| LintCore			-- ^ Lint the core program.

	-- optimisation
	| OptAll			-- ^ Perform all optimistaions.
	| OptSimplify			-- ^ Do core simplification.
	| OptTailCall			-- ^ Do tail-call optimisation.

	-- code generation
	| Debug				-- ^ Compile generated C files with GNU debugging.
	| StaticRuntime			-- ^ Link against static runtime.
	| Profile			-- ^ Profile generated C files.
	| ProfileRuntime		-- ^ Link against profiled runtime.
	| ViaLLVM			-- ^ Use the LLVM backend.

	-- linker	
	| Link				-- ^ Perform linking.
	| LinkObj	[String]	-- ^ Extra object to link with.
	| LinkLib	[String]	-- ^ Extra library to link with.
	| LinkLibDir	[String]	-- ^ Dir to search for extra link libraries.

	-- partial compilation
	| StopConstraint		-- ^ Stop after generating type constraints.
	| StopType			-- ^ Stop after solving type constraints.
	| StopCore			-- ^ Stop after producing core program.
	| StopSea			-- ^ Stop after producing C program.
	| StopCompile			-- ^ stop after compiling C program, no link.

	| StopErrors   [FilePath]	-- ^ On error, dump error messages to the given
					--   file names and succeed.

	| KeepCFiles			-- ^ Keep intermediate .c files.
	| KeepOFiles			-- ^ Keep intermediate .o files.

	-- Compiler debugging.
	| DebugGeneraliseDangerousVars	-- ^ Generalise dangrous type variables.
	| DebugNoConstraintSimplifier	-- ^ Don't run the constraint simplifier before solving.

	-- Dump flags ---------------------------------------------------------
	-- NOTE: If you add something here then make sure it's also present
	--       in the expansion for DumpAll in the expandArgs function below.
	--
	
	-- dump pretty printing options 
	| DumpPrettyUnique		-- ^ Show unique binders on variables.
	| DumpPrettyTypeSpaces		-- ^ Use * as a namespace qualifier for type variables.
	| DumpPrettyTypeKinds		-- ^ Show kinds of type variables.
	| DumpPrettyCoreTypes		-- ^ Show types on variables in core program.
	| DumpPrettyCoreMore		-- ^ Show more-than constraints on effect variables in core program.
	| DumpPrettySeaTypes		-- ^ Show types on variables in the sea program.

	-- dump everything
	| DumpAll			-- ^ Dump everything.

	| DumpNewInterfaces		-- ^ Dump the new style interface file.

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
	| DumpCoreTidy
	| DumpCoreSnip
	| DumpCoreClean
	| DumpCoreThread
	| DumpCoreLint
	| DumpCoreDict
	| DumpCoreBind
	| DumpCorePrim
	| DumpCoreSimplify
	| DumpCoreLift
	| DumpCorePrep
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

	Make{}
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

	-- we can't use the default top-level handler if the prelude isn't imported
	NoImplicitPrelude
	 -> x : NoImplicitHandler : expandArgs xs

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
	   , DumpCoreTidy
	   , DumpCoreSnip
	   , DumpCoreClean
	   , DumpCoreThread
	   , DumpCoreLint
	   , DumpCoreDict
	   , DumpCoreBind
	   , DumpCorePrim
	   , DumpCoreSimplify
	   , DumpCoreLift
	   , DumpCoreCurry
	   , DumpCorePrep

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


-- | Convert an Arg to the PrettyMode it represents (if any).
takePrettyModeOfArg :: Arg -> Maybe PrettyMode
takePrettyModeOfArg aa
 = case aa of
 	DumpPrettyUnique	-> Just $ PrettyUnique
	DumpPrettyTypeSpaces	-> Just $ PrettyTypeSpaces
	DumpPrettyTypeKinds	-> Just $ PrettyTypeKinds
	DumpPrettyCoreTypes	-> Just $ PrettyCoreTypes
	DumpPrettyCoreMore	-> Just $ PrettyCoreMore
	DumpPrettySeaTypes	-> Just $ PrettySeaTypes
	_			-> Nothing
