
module Main.Result
	(Result(..))
where
import DDC.Var


-- | Compilation result. 
--   We get one of these back for every module we successfully compile.
--   It should describe all the products of the compilation, including
--   all dropped files.
data Result
	= Result
	{ 
 	-- | Id of the compiled module.
	  resultModuleId	:: ModuleId
	
	-- | Whether the module defines the main function
	, resultDefinesMain	:: Bool

	-- | Path to compiled source .ds file.
	, resultSourceDS	:: FilePath

	-- | Path to produced .di file
	, resultOutputDI	:: FilePath

	-- | Path to produced .ddc.c file
	, resultOutputC		:: Maybe FilePath

	-- | Path to produced .ddc.h file
	, resultOutputH		:: Maybe FilePath

	-- | Path to produced .o file
	, resultOutputO		:: Maybe FilePath

	-- | Path to produced executable
	, resultOutputExe	:: Maybe FilePath }
