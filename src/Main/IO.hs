
module Main.IO
(
	ImportDef(..),
	chaseLinkObjs,
	chaseModules,
	chooseModuleName,
)

where

-----
import System.Cmd
import System.Directory			(doesFileExist)

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

-----
import Util

-----
import qualified Shared.Var		as Var
import Shared.Var			(Var, Module(..))

import Shared.Error			(panic)
import Shared.Pretty

import qualified Module.IO		as MIO
import qualified Module.Export		as ME

import qualified Source.Slurp		as S
import qualified Source.Exp		as S
import qualified Source.Pretty		as S
import qualified Source.Lexer		as S
import qualified Source.Parser		as S
import qualified Source.Token		as Token
import qualified Source.Pragma		as Pragma

import qualified Type.Exp		as T
import Type.Pretty

import Main.Path
import Main.Arg

-----
stage	= "Stages.IO"


data ImportDef
	= ImportDef
	{ idModule		:: Module
	, idFilePathDI		:: FilePath
	, idFilePathO		:: FilePath
	, idInterface		:: S.Tree
	, idImportedModules	:: [Module]
	, idLinkObjs		:: [FilePath] }

-----
chaseLinkObjs
	:: (?args :: [Arg])
	-> [FilePath]			-- directory roots to search for extra objects.
	-> [FilePath]			-- the list of object paths
	-> IO [FilePath]
	
chaseLinkObjs
	importDirs
	objectPaths
 = 	mapM (findLinkObj importDirs) objectPaths

findLinkObj importDirs objectPath
 = do	mObj	<- findFile importDirs objectPath
 	return	$ fromMaybe (deathLinkObj importDirs objectPath) 
		$ liftM snd mObj

deathLinkObj importDirs file
 = panic stage
 	$ "chaseLinkObjs: cannot find external object.\n" 
	% "    file       = '" % file 		% "'\n"
	% "    importDirs = '" % importDirs	% "'\n"
	
-----
chaseModules 
	:: (?args :: [Arg])
	-> [FilePath]			-- directories to search for imports.
	-> [Module]			-- the root list of modules
	-> IO (Map Module ImportDef)

chaseModules
	importDirs
	importsRoot
 = 	chaseModules' importDirs importsRoot Map.empty


chaseModules'
	importDirs
	importsRoot
	importMap

	| []		<- importsRoot
	= return importMap

	| (r:rs)	<- importsRoot
	, elem r $ Map.keys importMap
	= chaseModules' importDirs rs importMap
 
	| (r:rs)	<- importsRoot
	= do	mInt	<- loadInterface importDirs r
		case mInt of
		 Nothing	
	   	  -> panic stage
	  		$ "chase: cannot find interface file for module " % r
		
	  	 Just int
		  -> chaseModules' 
		  	importDirs 
			(idImportedModules int ++ rs)
		 	(Map.insert r int $ importMap)
	 

-----------------------
-- loadInterface
--	Find, read and parse a module interface.
--
loadInterface 
	:: (?args :: [Arg])
	-> [FilePath]
	-> Module
	-> IO (Maybe ImportDef)

loadInterface
	importDirs
	mod
 = do
 	let ModuleAbsolute vs	= mod
	let fileDir		= pretty $ "/" %!% vs
	let fileNameDI		= fileDir ++ ".di"
	let fileNameO		= fileDir ++ ".o"

	let fileBase		= pretty $ "/" %!% init vs
		
	mPathDI			<- findFile importDirs fileNameDI
	mPathO			<- findFile importDirs fileNameO
	
	loadInterface' importDirs fileBase mod mPathDI mPathO
	
loadInterface' importDirs fileDir mod mPathDI mPathO
	| Just (dirDI, pathDI)	<- mPathDI
	, Just (dirO,  pathO)	<- mPathO
	= do
		source		<- readFile pathDI
		let tree
			= S.parse 
			$ map (\t -> t { Token.file = pathDI })
			$ S.scan source
		
		linkObjs
			<- chaseLinkObjs 
				[i ++ "/" ++ fileDir | i <- importDirs]
				(Pragma.slurpLinkObjs tree)

		returnJ	
			$ ImportDef
			{ idModule		= mod
			, idFilePathDI		= pathDI
			, idFilePathO		= pathO
			, idInterface		= tree
			, idImportedModules	= S.slurpImportModules tree
			, idLinkObjs		= linkObjs }

	| otherwise
	= return Nothing

-----
findFile 
	:: [FilePath] 
	-> String
	-> IO (Maybe (FilePath, FilePath))

findFile importDirs name
 	| []		<- importDirs
 	= return Nothing
 
 	| (d:ds)	<- importDirs
 	= do	let testName	= d ++ "/" ++ name
		exists	<- doesFileExist testName
		if exists
		 then	return $ Just (d, testName)
		 else	findFile ds name

-----
-- chooseModuleName
--	Choose a name for a module based on its file path and a list
--	of import dirs.
--
--	eg, 	for file 	"library/Lib/Data/Thing.ds", imports ["library"]
--	choose module name 	"Lib.Data.Thing"
--
chooseModuleName 
	:: [FilePath]		-- import dirs
	-> FilePath		-- filename of module
	-> Maybe Module
	
chooseModuleName iDirs fileName
	= takeFirstJust 
	$ map (matchName fileName) iDirs
	
matchName ('/':aa)	[]	= Just $ munchModule aa
matchName (a:aa) 	(b:bb)
	| a == b		= matchName aa bb
	| otherwise		= Just $ ModuleAbsolute ["Main"]

munchModule s					-- eg 	"Dir1/Dir2/File.ds"
 = let	
 	sMunch		= breakOns '/' s	--	["Dir1", "Dir2", "File.ds"]
 	dirParts	= init sMunch		--	["Dir1", "Dir2"]

	Just filePart	= takeLast sMunch	--	"File.ds"
	fileMunch	= breakOns '.' filePart	--	["File", "ds"]
	Just file	= takeHead fileMunch	-- 	"File"
	
   in	ModuleAbsolute 
   		$ dirParts ++ [file]
	
