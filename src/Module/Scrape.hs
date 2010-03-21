
-- A scrape contains useful about a module that we can derive directly from the
--	source, interface and build files without properly parsing or type checking it.
--
module Module.Scrape
	( Scrape (..)
	, scrapeSourceFile
	, scrapeModule)
where
import Main.BuildFile
import Source.Parser.Base
import Source.Parser.Module
import Source.Lexer
import Source.Exp
import Util
import Util.FilePath
import Util.System.Directory
import System.IO
import System.Directory
import Text.Regex
import Data.Char
import Shared.Pretty					()
import Shared.Var					(Module(..))
import qualified System
import qualified Main.Arg				as Arg
import qualified Text.ParserCombinators.Parsec.Prim	as Parsec


-- Scrape -----------------------------------------------------------------------------------------
data Scrape
	= Scrape
	-- the module name, derived from its file path
	{ scrapeModuleName	:: Module

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
	, scrapeImported	:: [Module]

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


-- ScrapeSource -----------------------------------------------------------------------------------

-- Find a module based on its file name and scrape it
--	Raises System.exitFailure if the file does not exist
--
scrapeSourceFile
	:: [FilePath]		-- Directories to look for imported modules in
	-> Bool			-- Whether the Prelude module is being auto imported
				--	This can be overridden by pragmas in the source file.
	-> FilePath		-- path to source file
	-> IO (Maybe Scrape)
	
scrapeSourceFile importDirs importPrelude pathSource
 = do	exists	<- doesFileExist pathSource
	if not exists
	 then do
		hPutStrLn stderr $ "ddc error: File '" ++ pathSource ++ "' does not exist.\n"
		System.exitFailure
	 else do
		source	<- readFile pathSource
		scrapeSourceFile' importDirs importPrelude pathSource source
		
scrapeSourceFile' importDirs importPrelude pathSource source
 = do	
	-- split up the path to the module
	let (fileName, fileDir, fileBase, _)
		= normalMunchFilePath pathSource

	-- Also look for imports in the dir the module is in
	let importDirs'	
		= fileDir : importDirs

	-- Choose a name for this module.
{-	let Just moduleName	
		= chooseModuleName importDirs' fileName

	putStr
	 $  "moduleName " ++ show moduleName ++ "\n"
	 ++ "importDirs " ++ show importDirs' ++ "\n"
	 ++ "fileName   " ++ show fileName	++ "\n"
	 
-}
	scrapeModule' 
		Nothing
		importDirs' importPrelude
		Nothing fileDir fileName fileBase


-- ScrapeModule -----------------------------------------------------------------------------------
-- Use the import dirs to find the source file for a particular module, 
--	then and scrape it to work out whether we have to rebuild it or not.
--
scrapeModule
	:: [FilePath]		-- Directories to look for imported modules in.
	-> Bool			-- Whether the Prelude module is being auto imported
				--	This can be overridden by pragmas in the source file.
	-> Module		-- The name of the module to scrape.
	-> IO (Maybe Scrape)
	
scrapeModule importDirs importPrelude moduleNameSearchedFor
 = do	-- get the base path of a module by replacing '.' by '/'
	let ModuleAbsolute vs	= moduleNameSearchedFor
	let fileNameDS		= catInt "/" vs ++ ".ds"

	mFileDirName	<- findFileInDirs importDirs fileNameDS
	
	case mFileDirName of
	 Nothing	-> return Nothing
	 Just (importDir, fileNameFound)	
	  -> do	
		let (fileName, fileDir, fileBase, _)
			= normalMunchFilePath fileNameFound

		scrapeModule' 
			(Just moduleNameSearchedFor)
			importDirs importPrelude
			(Just importDir) fileDir fileName fileBase

scrapeModule' 
	(mModuleNameSearchedFor :: Maybe Module)
	importDirs importPrelude
	mImportDir fileDir fileName fileBase
 = do	
	-- See if there is a build file
	--	For some source file,             ./Thing.ds
	--	the build file should be called   ./Thing.build
	let buildFile	= fileDir ++ "/" ++ fileBase ++ ".build"
	mBuild		<- loadBuildFile  buildFile

	-- Read the file
	source	<- readFile fileName

	let sourceLines_noComments
		= lines $ dropStrComments source

	-- Scrape the module name directly from the source
	let mModuleScraped	
		= scrapeModuleId sourceLines_noComments

	let moduleName
		-- Always believe the module id given in the source file.
		| Just m	<- mModuleScraped
		= m

		-- If there is no id in the source file, but we found a specific module
		--	in the appropriate place, then we already have the module id.
		| Just m	<- mModuleNameSearchedFor
		= m

		-- If neither of the above apply, then derive the module id from the file name.
		| otherwise					
		= ModuleAbsolute [fileBase]

{-
	putStr 	$  "fileName           = " ++ show fileName 			++ "\n"
		++ "mModuleSearchedFor = " ++ show mModuleNameSearchedFor 	++ "\n"
		++ "mModuleScraped     = " ++ show mModuleScraped 		++ "\n"
		++ "fileBase           = " ++ show fileBase 			++ "\n"
		++ "chosen modulename  = " ++ show moduleName			++ "\n\n"
-}
	-- Scrape the inline options from the source file
	let options	= scrapeOptions source
	let inlineArgs	= Arg.parse options

	-- Scrape the imported modules from the source file
	let importMods	= scrapeImports sourceLines_noComments
	
	-- Auto-import the Prelude unless something's telling us not to
	let importModsPrelude
		= if   importPrelude
		    && (not $ elem Arg.NoImplicitPrelude inlineArgs)
			then nub (importMods ++ [ModuleAbsolute ["Prelude"]])
			else importMods
			
	-- See if there is an interface file
	let intFile	= fileDir ++ "/" ++ fileBase ++ ".di"
	intExists	<- doesFileExist intFile
	let mInterface	= if intExists then Just intFile else Nothing

	-- See if there is a header file
	let headerFile	= fileDir ++ "/" ++ fileBase ++ ".ddc.h"
	headerExists	<- doesFileExist headerFile
	let mHeader	= if headerExists then Just headerFile else Nothing

	-- See if there is an object file
	let objFile	= fileDir ++ "/" ++ fileBase ++ ".o"
	objExists	<- doesFileExist objFile
	let mObject	= if objExists then Just objFile else Nothing
		
	-- Decide whether the module needs (re)building 
	needsRebuild	<- checkNeedsRebuild (Just fileName) mInterface	mHeader mObject
		
  	return	$ Just $ Scrape
		{ scrapeModuleName	= moduleName
		, scrapePathSource	= Just fileName
		, scrapePathInterface	= mInterface
		, scrapePathHeader	= mHeader
		, scrapePathObject	= mObject
		, scrapeImportDir	= mImportDir
		, scrapeNeedsRebuild	= needsRebuild
		, scrapeImported	= importModsPrelude
		, scrapeArgsInline	= inlineArgs
		, scrapeBuild		= mBuild 
		, scrapeDefinesMain	= False }

checkNeedsRebuild mSource mInt mHeader mObj
	-- module not found
	| isNothing mSource
	, isNothing mInt
	= return False
	
	-- source file but no interface, or object, rebuild
	| isJust mSource
	, isNothing mInt ||  isNothing mHeader || isNothing mObj 
	= return True
	
	-- interface but no source.. can't rebuild
	| isNothing mSource
	, isJust mInt
	, isJust mObj
	= return False
	
	-- check that interface is as recent as source
	| Just fileSource	<- mSource
	, Just fileInt		<- mInt
	= do	timeSource	<- getModificationTime fileSource
		timeInt		<- getModificationTime fileInt
		return	(timeInt < timeSource)


-- ScrapeImports ----------------------------------------------------------------------------------
-- | Scrape the list of imported modules directly from a source file.
--	This needs to be fast, as we need to scrape all the source files in a project
--	every time we do a make.
--
scrapeImports :: [String] -> [Module]
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
	 Right (PImportModule sp mods)	-> mods
	

-- ScrapeModuleName -------------------------------------------------------------------------------
scrapeModuleId :: [String] -> Maybe Module
scrapeModuleId []	= Nothing
scrapeModuleId (l:ls)
 = case words l of
	 "module" : name : _	-> Just (ModuleAbsolute (breakOns '.' name))
	 _			-> scrapeModuleId ls

-- ScrapeOptions ----------------------------------------------------------------------------------
-- | Scrape options from the source file.
--	Options are treated as though they were passed to the compiler 
--	on the command line.
--
scrapeOptions :: String -> [String]
scrapeOptions source
 	= scrapeOptions' (lines source)

scrapeOptions' [] = []
scrapeOptions' (l:ls)
	| isPrefixOf "{-# OPTIONS" l ||
	  isPrefixOf "{-#\tOPTIONS" l
	= let	regex		= mkRegex "\\{-#[ \\t]+OPTIONS[ \\t]+(.*)#-\\}"
		Just [str]	= matchRegex regex l
	  in	words str ++ scrapeOptions' ls
	
	| otherwise
	= scrapeOptions' ls
	
	
