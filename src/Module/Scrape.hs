
module Module.Scrape
	( Scrape (..)
	, scrapeRecursive
	, scrapeModule 
	, scrapeSourceFile)
where

import qualified Main.Arg	as Arg
import Main.Setup
import Main.IO
import Main.Build

import Module.IO

import Source.Parser.Base
import Source.Parser.Module
import Source.Lexer
import Source.Exp

import Shared.Var		(Var, Module(..))
import Shared.Pretty

import Text.Regex
import qualified Text.ParserCombinators.Parsec.Prim	as Parsec
import qualified Data.Map	as Map
import Data.Map			(Map)

import System.IO
import System.Directory
import System.Exit
import Data.String
import Data.Char



import Util

-- A scrape contains some useful about a module that we can derive directly from the
--	source, interface and build files. No need to properly parse, or type check the source.
--
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
	, scrapeBuild		:: Maybe Build }
	deriving (Show)


-- Scrape all modules reachable from these ones
scrapeRecursive
	:: Setup
	-> [Scrape]			-- root set
	-> IO (Map Module Scrape)	-- graph of modules reachable from, this one

scrapeRecursive setup roots
 = let	graph	= Map.fromList [ (scrapeModuleName s, s) | s <- roots]

	-- each child carries the scrape of the module that imported it.
	--	this is used for error reporting incase we can't find the child
	msImp	= catMap 
			(\s -> [(s, imp)
					| imp <- scrapeImported s])
			roots

   in 	scrapeRecursive' setup  graph msImp

scrapeRecursive' setup graph []
	= return graph
	
scrapeRecursive' setup graph ((sParent, v):vs)
	-- this module is already in the graph
	| isJust $ Map.lookup v graph
	= scrapeRecursive' setup graph vs
	
	-- scrape module and add its children
	| otherwise
	= do	mScrapeChild	<- scrapeModule setup v
		case mScrapeChild of
		 Nothing
		  -> do	let Just pathSource	= scrapePathSource sParent
		  	putStr	$ pprStrPlain 
		  		$ "ddc error: can't find source for module '" % v % "'\n"
		  		% "    imported by: " % pathSource % "\n\n"
		  		
			exitFailure
			
		 Just sChild
		  ->	scrapeRecursive' 
				setup 
				(Map.insert v sChild graph)
				( [(sChild, imp)
					| imp <- scrapeImported sChild]
				  ++ vs)


-- Use the import dirs to file the source file for a particular module
--	and scrape it
scrapeModule
	:: Setup
	-> Module
	-> IO (Maybe Scrape)
	
scrapeModule setup moduleName
 = do	-- get the base path of a module by replacing '.' by '/'
	let ModuleAbsolute vs	= moduleName
	let fileNameDS		= catInt "/" vs ++ ".ds"

	-- Gather up all the import dirs.
	let importDirs	
		= (setupLibrary setup)
		: (concat $ [dirs | Arg.ImportDirs dirs <- setupArgs setup])

	mFileDirName	<- findFile importDirs fileNameDS
	
	case mFileDirName of
	 Nothing			-> return Nothing
	 Just (importDir, fileNameFound)	
	  -> do	
		let (fileName, fileDir, fileBase, _)
			= normaliseFileName fileNameFound

		scrapeModule' setup moduleName (Just importDir) fileDir fileName fileBase


-- Scrape this module source file
--	returns Nothing if the file does not exist
scrapeSourceFile
	:: Setup
	-> FilePath		-- path to source file
	-> IO (Maybe Scrape)
	
scrapeSourceFile setup pathSource
 = do	exists	<- doesFileExist pathSource
	if not exists
	 then return Nothing
	 else do
		source	<- readFile pathSource
		scrapeSourceFile' setup pathSource source
		
scrapeSourceFile' setup pathSource source
 = do	-- decide on a name for this module
	let (fileName, fileDir, fileBase, _)
		= normaliseFileName pathSource

	-- Gather up all the import dirs.
	let importDirs	
		= setupLibrary setup
		: fileDir
		: (concat [dirs | Arg.ImportDirs dirs <- setupArgs setup])

	-- Choose a name for this module.
	let Just moduleName	
		= chooseModuleName importDirs fileName

	scrapeModule' setup moduleName Nothing fileDir fileName fileBase

scrapeModule' setup moduleName mImportDir fileDir fileName fileBase
 = do	
	-- See if there is a build file
	--	For some source file,             ./Thing.ds
	--	the build file should be called   ./Thing.build
	let buildFile	= fileDir ++ "/" ++ fileBase ++ ".build"
	mBuild		<- loadBuildFile  buildFile

	-- Read the file
	source		<- readFile fileName

	-- Scrape the inline options from the source file
	let options	= scrapeOptions source
	let inlineArgs	= Arg.parse (catInt " " options)

	-- Scrape the imported modules from the source file
	let importMods	= scrapeImports source
	
	-- Auto-import the Prelude unless something's telling us not to
	let importModsPrelude
		= if elem Arg.NoImplicitPrelude 
			(setupArgs setup ++ inlineArgs)
		   then importMods
		   else nub (importMods ++ [ModuleAbsolute ["Prelude"]])

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
		, scrapeBuild		= mBuild }


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
	
	
-- | Scrape the list of imported modules directly from a source file.
--	This needs to be fast, as we need to scrape all the source files in a project
--	every time we do a make.

scrapeImports :: String -> [Module]
scrapeImports source
	= scrapeImports' (lines source)

scrapeImports' [] = []

scrapeImports' (l:ls)
	| isPrefixOf "import " l  ||
	  isPrefixOf "import\t" l 
 	= let	
		-- gather up multi-line imports
		endLine	(c : _)	
			| isSpace c	= False
		endLine _		= True
		
		(impRest, ls')	= splitWhenLeft endLine ls
		
	  in	scrapeImport (l : impRest) ++ scrapeImports' ls'
	
	| otherwise
	= scrapeImports' ls
			
scrapeImport ls
 = let	tokens	= fst $ scanModuleWithOffside (catInt "\n" (ls ++ ["\n"]))
	result	= Parsec.runParser (pCParen pTopImport) () "" tokens
	
   in	case result of
	 -- if we get a parse error then just return an empty scrape
	 -- the compiler will give an error when we get to that module anyway
	 Left err			-> error (show err)
	 Right (PImportModule sp mods)	-> mods
	
	
-- | Scrape pragmas from the source file.
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
	
	
