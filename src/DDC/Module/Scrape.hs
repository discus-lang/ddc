{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Module.Scrape
	( Scrape (..)
	, scrapeSourceFile
	, scrapeSourceModule)
where
import Main.BuildFile
import Source.Parser.Base
import Source.Parser.Module
import Source.Lexer
import Source.Exp
import DDC.Var
import DDC.Main.Error
import DDC.Util.Text
import Data.Char
import Util.System.Directory
import Util
import System.IO
import System.Directory
import System.FilePath
import DDC.Main.Pretty					()
import qualified DDC.Main.Arg				as Arg
import qualified DDC.Main.ParseArgs			as Arg
import qualified System.Exit				as System
import qualified Text.ParserCombinators.Parsec.Prim	as Parsec

stage = "DDC.Module.Scrape"

-- | A scrape contains useful about a module that we can derive directly from the
--   source, interface and build files without properly parsing or type checking it.
data Scrape
	= Scrape
	-- the module name, derived from its file path
	{ scrapeModuleName	:: ModuleId

	-- path to the source file (if found)
	, scrapePathSource	:: Maybe FilePath

	-- path to the interface file (if found)
	, scrapePathInterface	:: Maybe FilePath

	-- path to the sea header file (if found)
	, scrapePathHeader	:: Maybe FilePath

	-- path to the object file (if found)
	, scrapePathObject	:: Maybe FilePath

	-- the import dir that was used to find this module, 
	--	or Nothing if it was via a direct file path
	, scrapeImportDir	:: Maybe FilePath
	
	-- modules that this one imports, scraped from the source
	, scrapeImported	:: [ModuleId]

	-- extra args from inline OPTIONS pragams
	, scrapeArgsInline	:: [Arg.Arg]
	
	-- whether this module needs rebuilding,
	--	determined from source vs interface timestamps
	, scrapeNeedsRebuild	:: Bool

	-- contents of build file for this module (if it exists)
	, scrapeBuild		:: Maybe Build 
	
	-- Defines the main function 
	, scrapeDefinesMain	:: Bool }
	deriving (Show)


-- | Read a module from a file and scrape it.
--   Raises System.exitFailure if the file does not exist
scrapeSourceFile
	:: Bool			-- ^ Whether to implicitly import the Prelude.
	-> FilePath		-- ^ Path to source file (need not be canonical)
	-> IO (Maybe Scrape)
	
scrapeSourceFile shouldImportPrelude pathSource_
 = do	pathSource	<- canonicalizePath pathSource_
	exists		<- doesFileExist pathSource

	if not exists
	 then do
		dirWorking	 <- getCurrentDirectory
		let pathRelative = makeRelative dirWorking pathSource_
		hPutStrLn stderr $ "ddc error: File '" ++ pathRelative ++ "' does not exist.\n"
		System.exitFailure

	 else	liftM Just $ scrapeModuleFromFile Nothing shouldImportPrelude pathSource

	
-- | Find a module based on its name, and scrape out some info about it.
scrapeSourceModule
	:: [FilePath]		-- ^ Directories to search for imports.
	-> Bool			-- ^ Whether we should implicitly import the prelude.a
	-> ModuleId		-- ^ The name of the module to find.
	-> IO (Maybe Scrape)

scrapeSourceModule impDirs shouldImportPrelude modName
 = do	
	-- Look for the module in a dir matching it's name.
	-- eg, it the module is     SomeLib.SomeThing.ModuleName
	--     then look for it in  <some import dir>/SomeLib/SomeThing/ModuleName
	let ModuleId vs		= modName
	let fileNameDS		= catInt "/" vs ++ ".ds"
	mFileDirName		<- findFileInDirs impDirs fileNameDS
	
	-- Did we find it?
	case mFileDirName of
	 Nothing	-> return Nothing
	 Just (_impDir, pathFileFound_)	
	  -> do	pathFileFound	<- canonicalizePath pathFileFound_
		liftM Just $ scrapeModuleFromFile (Just modName) shouldImportPrelude pathFileFound


-- | Scrape a module from a given file.
--   The file must exist else 'panic'.
scrapeModuleFromFile
	:: Maybe ModuleId	-- ^ Module name that we think this file should have.
	-> Bool			-- ^ Whether we should implicitly import the prelude.
	-> FilePath		-- ^ Name of .ds file holding module (need not be canonical)
	-> IO Scrape
	
scrapeModuleFromFile mModuleNameSearchedFor shouldImportPrelude filePath_
 = do	filePath	<- canonicalizePath filePath_
	exists		<- doesFileExist filePath
	when (not exists)
	 $ panic stage $ "scrapeModuleFromFile: no such file " ++ filePath
	
	let fileDir	= takeDirectory filePath
	let fileBase	= takeBaseName  filePath
		
	-- See if there is a build file
	--	For some source file,             ./Thing.ds
	--	the build file should be called   ./Thing.build
	let buildFile	= replaceExtension filePath "build"
	mBuild		<- loadBuildFile  buildFile

	-- Read the source file.
	source	<- readFile filePath

	let sourceLines_noComments
		= lines $ dropStrComments source

	-- Scrape the module name directly from the source
	let mModuleScraped	
		= scrapeModuleId sourceLines_noComments

	-- Decide on a module name for this source file.
	let modName
		-- Always believe the module id given in the source file.
		| Just m <- mModuleScraped	   = m

		-- If there is no id in the source file, but we found a specific module
		--	in the appropriate place, then we already have the module id.
		| Just m <- mModuleNameSearchedFor = m

		-- If neither of the above apply, then derive the module id from the file name.
		| otherwise			   = ModuleId [fileBase]

	-- Scrape the inline options from the source file
	let options	= scrapeOptions source
	let inlineArgs	= Arg.parse options

	-- Scrape the imported modules from the source file
	let impModules	= scrapeImports sourceLines_noComments
	
	-- Auto-import the Prelude unless something's telling us not to
	let impModulesPrelude
		= if   shouldImportPrelude
		    && (not $ elem Arg.NoImplicitPrelude inlineArgs)
			then nub (impModules ++ [ModuleId ["Prelude"]])
			else impModules
			
	-- See if there is an interface file
	let intFile	= replaceExtension filePath ".di"
	intExists	<- doesFileExist intFile
	let mInterface	= if intExists then Just intFile else Nothing

	-- See if there is a header file
	let headerFile	= replaceExtension filePath ".ddc.h"
	headerExists	<- doesFileExist headerFile
	let mHeader	= if headerExists then Just headerFile else Nothing

	-- See if there is an object file
	let objFile	= replaceExtension filePath ".o"
	objExists	<- doesFileExist objFile
	let mObject	= if objExists then Just objFile else Nothing
		
	-- Decide whether the module needs (re)building 
	needsRebuilds	 <- mapM (isMoreRecentOrMissing filePath) 
				[mInterface, mHeader, mObject]
				
	let needsRebuild = or needsRebuilds
		
  	return	$ Scrape
		{ scrapeModuleName	= modName
		, scrapePathSource	= Just filePath
		, scrapePathInterface	= mInterface
		, scrapePathHeader	= mHeader
		, scrapePathObject	= mObject
		, scrapeImportDir	= Just fileDir
		, scrapeNeedsRebuild	= needsRebuild
		, scrapeImported	= impModulesPrelude
		, scrapeArgsInline	= inlineArgs
		, scrapeBuild		= mBuild 
		, scrapeDefinesMain	= False }


isMoreRecentOrMissing :: FilePath -> Maybe FilePath -> IO Bool
isMoreRecentOrMissing file1 mFile2
 = case mFile2 of
	Nothing	
	 -> return True

	Just file2
	 -> do	time1	<- getModificationTime file1	
		time2	<- getModificationTime file2
		return	$ time1 > time2


-- | Scrape the list of imported modules directly from a source file.
--	This needs to be fast, as we need to scrape all the source files in a project
--	every time we do a make.
scrapeImports :: [String] -> [ModuleId]
scrapeImports [] = []
scrapeImports (l:ls)
	| isPrefixOf "import " l  ||
	  isPrefixOf "import\t" l 
 	= let	
		-- gather up multi-line imports
		endLine	(c : _)	
			| isSpace c	= False
		endLine _		= True
		
		(impRest, ls')	= splitWhenLeft endLine ls
		
	  in	scrapeImport (l : impRest) ++ scrapeImports ls'
	
	| otherwise
	= scrapeImports ls
			
scrapeImport ls
 = let	tokens	= fst $ scanModuleWithOffside (catInt "\n" (ls ++ ["\n"]))
	result	= Parsec.runParser (pCParen pTopImport) () "" tokens
	
   in	case result of
	 -- if we get a parse error then just return an empty scrape
	 -- the compiler will give an error when we get to that module anyway
	 Left err			-> error (show err)
	 Right (PImportModule _ mods)	-> mods
	 Right _			-> panic stage $ "scrapeImport: unexpected pragma"


-- | Scrape the module id from the source file.
scrapeModuleId :: [String] -> Maybe ModuleId
scrapeModuleId []	= Nothing
scrapeModuleId (l:ls)
 = case words l of
	 "module" : name : _	-> Just (ModuleId (breakOns '.' name))
	 _			-> scrapeModuleId ls


-- | Scrape options from the source file.
--	Options are treated as though they were passed to the compiler 
--	on the command line.
scrapeOptions :: String -> [String]
scrapeOptions source
 	= scrapeOptions' (lines source)

scrapeOptions' [] = []
scrapeOptions' (l:ls)
	| l'		<- normaliseSpaces l
	, Just rest	<- stripPrefix "{-# OPTIONS " l'
	, params	<- takeWhile (not . (== '#')) rest
	= words params ++ scrapeOptions' ls
	
	| otherwise
	= scrapeOptions' ls

