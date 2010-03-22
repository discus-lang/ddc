
module Main.IO
	( ImportDef(..)
	, chaseModules)
where
import Main.Setup
import Main.Arg
import Shared.Error
import Util
import Util.System.Directory
import DDC.Main.Pretty
import DDC.Base.SourcePos
import Shared.Var			(ModuleId(..))
import qualified Data.Map		as Map
import qualified Source.Slurp		as S
import qualified Source.Exp		as S
import qualified Source.Lexer		as S
import qualified Source.Parser.Module	as S
import qualified Source.Token		as Token

-----
stage	= "Main.IO"

-- | Where an imported modules files are
data ImportDef
	= ImportDef
	{ -- | The module name
	  idModule		:: ModuleId

	  -- | Full path to its .di interface file.
	, idFilePathDI		:: FilePath

	  -- | Full path to its .o object file. 
	, idFilePathO		:: FilePath

	  -- | Relative path between source .ds and .ddc.h header
	, idFileRelPathH	:: FilePath	

	  -- | The (parsed) interface.
	, idInterface		:: S.Tree SourcePos

	  -- | Other modules imported by this one.
	, idImportedModules	:: [ModuleId] }


-- | Chase down interface files for these modules.
--	Do this recursively and return a map with all the interfaces needed.
--
chaseModules 
	:: Setup				-- ^ current compile setup
	-> (Setup -> FilePath -> IO ())		-- ^ function to compile a module.
	-> [FilePath]				-- ^ directories to search for imports.
	-> [ModuleId]				-- ^ the root list of modules.
	-> Map ModuleId ImportDef		-- ^ the map to add module interfaces to.
	-> IO (Map ModuleId ImportDef)

chaseModules setup compileFun importDirs importsRoot importMap

	-- no more modules to find, all done.
	| []		<- importsRoot
	= return importMap

	-- this module is already in the map.
	| (r:rs)	<- importsRoot
	, elem r $ Map.keys importMap
	= chaseModules setup compileFun importDirs rs importMap
 
	-- try and find the interface for this module
	| (r:rs)	<- importsRoot
	= do	mInt	<- loadInterface (setupArgs setup) importDirs r

		case mInt of
		 -- add the interface file to the map
	  	 Just int
		  -> chaseModules setup compileFun importDirs 
			(idImportedModules int ++ rs)
		 	(Map.insert r int $ importMap)

		 -- we haven't got an interface file, maybe we can build the source..
		 Nothing -> chaseModules_build setup compileFun importDirs importsRoot importMap

chaseModules_die missingModule importDirs
 = dieWithUserError
	[ "    Can't find interface file for module '" % missingModule % "'\n"
	% "    Import dirs are:\n" 
	%> punc "\n" importDirs 	% "\n" ]

chaseModules_build setup compileFun importDirs importsRoot importMap
	
	| (r : rs)	<- importsRoot
	= do 	let ModuleId vs		= r
		let fileDir		= pprStrPlain $ "/" %!% vs
		let fileNameDS		= fileDir ++ ".ds"
		
		-- try and find source for this module
		mPathDS	<- findFileInDirs importDirs fileNameDS
		
		case mPathDS of 
                 -- build the source file.
		 Just (_, pathDS)	
		  -> do	
		  	-- TODO: check for recursive modules
		  	let setup'	= chaseModules_checkRecursive setup pathDS
						
		  	compileFun setup' pathDS

		  	mInt	<- loadInterface (setupArgs setup) importDirs r
			case mInt of 
			 Just int	-> chaseModules setup compileFun importDirs 
			 			(idImportedModules int ++ rs)
						(Map.insert r int $ importMap)

			 -- no interface file was dropped during compilation,
			 --	but the compile function didn't end the program.
			 Nothing	
			  -> panic stage 
			  		$ "chaseModules_build: source " % pathDS 
			  		% " compiled, but no interface file was dropped."
		  
		 -- no source and no interface, time to die.
		 Nothing		-> chaseModules_die r importDirs

chaseModules_checkRecursive setup fileName
	| Just fs	<- setupRecursive setup
	, elem fileName fs
	= dieWithUserError
		[ "    Can't compile recursive module '" % fileName % "'\n"
		% "      imports are:\n"
			%> punc "\n" fs % "\n"]
		
	| Just fs	<- setupRecursive setup
	= setup { setupRecursive = Just (fileName : fs) }
	
	| otherwise
	= setup { setupRecursive = Just [fileName] }


-- | find, read and parse a module interface file.
loadInterface 
	:: [Arg]
	-> [FilePath]
	-> ModuleId
	-> IO (Maybe ImportDef)

loadInterface
	args
	importDirs
	mod
 = do
 	let ModuleId vs		= mod
	let fileDir		= pprStrPlain $ "/" %!% vs
	let fileNameDI		= fileDir ++ ".di"
	let fileNameO		= fileDir ++ ".o"
	let fileNameH		= fileDir ++ ".ddc.h"

	mPathDI			<- findFileInDirs importDirs fileNameDI
	mPathO			<- findFileInDirs importDirs fileNameO
	mPathH			<- findFileInDirs importDirs fileNameH
	
	let result
		| Just (dirDI, pathDI)	<- mPathDI
		, Just (dirO,  pathO)	<- mPathO
		, Just (dirH,  pathH)	<- mPathH
		= do
			source		<- readFile pathDI
			let tree
				= S.parseModule pathDI 
				$ map (\t -> t { Token.tokenFile = pathDI })
				$ fst
				$ S.scanModuleWithOffside source
		
			returnJ	
				$ ImportDef
				{ idModule		= mod
				, idFilePathDI		= pathDI
				, idFilePathO		= pathO
				, idFileRelPathH	= chompFile $ dropPrefix [dirH] pathH
				, idInterface		= tree
				, idImportedModules	= S.slurpImportModules tree }

		| otherwise
		= return Nothing

	result

chompFile ('/':ss)	= ss
chompFile ss		= ss

dropPrefix [] x	= x
dropPrefix (p:ps) x
	| isPrefixOf p x	= drop (length p) x
	| otherwise		= dropPrefix ps x
	
