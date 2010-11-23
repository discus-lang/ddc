{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for compiler stages dealing with Sea code.
module Main.Sea
	( compileViaSea

	, seaSub
	, seaCtor
	, seaThunking
	, seaForce
	, seaSlot
	, seaFlatten
	, seaInit

	, makeSeaHeader )
where

-- main stages
import Main.Setup
import Main.BuildFile
import Main.Dump
import Main.Util

import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var

import qualified DDC.Module.Scrape	as M
import qualified DDC.Main.Arg		as Arg
import qualified DDC.Core.Glob		as C
import qualified DDC.Config.Version	as Version

-- sea
import qualified Sea.Util		as E

import DDC.Sea.Exp
import Sea.Util
import Sea.Invoke

import Sea.Sub		(subTree)
import Sea.Ctor		(expandCtorTree)
import Sea.Proto	(addSuperProtosTree)
import Sea.Thunk	(thunkTree)
import Sea.Force	(forceTree)
import Sea.Slot		(slotTree)
import Sea.Flatten	(flattenTree)
import DDC.Sea.Init	(initTree, mainTree)

import Util

-- haskell
import Data.Char
import qualified Data.Map		as Map


compileViaSea
	:: (?verbose :: Bool, ?pathSourceBase :: FilePath)
	=> Setup			-- ^ Compile setup.
	-> ModuleId			-- ^ Module to compile, must also be in the scrape graph.
	-> Tree ()			-- ^ The Tree for the module.
	-> FilePath			-- ^ FilePath of source file.
	-> [FilePath]			-- ^ C import directories.
	-> [FilePath]			-- ^ C include files.
	-> Map ModuleId [a]		-- ^ Module import map.
	-> Bool				-- ^ Module defines 'main' function.
	-> M.Scrape			-- ^ ScrapeGraph of this Module.
	-> Map ModuleId M.Scrape	-- ^ Scrape graph of all modules reachable from the root.
	-> Bool				-- ^ Whether to treat a 'main' function defined by this module
					--	as the program entry point.
	-> IO Bool

compileViaSea
	setup modName eInit pathSource importDirs includeFilesHere importsExp
	modDefinesMainFn sRoot scrapes_noRoot blessMain
 = do
	let ?args		= setupArgs setup

	-- Generate C source code ---------------------------------------------
	outVerb $ ppr $ "  * Generate C source code\n"
	(  seaHeader
	 , seaSource )	<- outSea
				modName
	 			eInit
				pathSource
				(map ((\(Just f) -> f) . M.scrapePathHeader)
					$ Map.elems scrapes_noRoot)
				includeFilesHere


	-- If this module binds the top level main function
	--	then append RTS initialisation code.
	seaSourceInit
		<- if modDefinesMainFn && blessMain
			then do mainCode <- seaMain
						(map fst $ Map.toList importsExp)
						modName

				return 	$ seaSource
					++ (catInt "\n" $ map pprStrPlain
							$ E.eraseAnnotsTree mainCode)

			else 	return  $ seaSource

	-- Write C files ------------------------------------------------------
	outVerb $ ppr $ "  * Write C files\n"
	writeFile (?pathSourceBase ++ ".ddc.c") seaSourceInit
	writeFile (?pathSourceBase ++ ".ddc.h") seaHeader


	------------------------------------------------------------------------
	-- Invoke external compiler / linker
	------------------------------------------------------------------------

	-- !! Early exit on StopSea
	when (elem Arg.StopSea ?args)
		compileExit

	-- Invoke GCC to compile C source.
	invokeSeaCompiler
		?args
		?pathSourceBase
		(setupRuntime setup)
		(setupLibrary setup)
		importDirs
		(fromMaybe [] $ liftM buildExtraCCFlags (M.scrapeBuild sRoot))

	return modDefinesMainFn

-- | Substitute trivial x1 = x2 bindings
seaSub
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())

seaSub tree
 = do
 	let tree'
		= subTree tree

	dumpET Arg.DumpSeaSub "sea-sub"
		$ eraseAnnotsTree tree'

	return tree'


-- | Expand code for data constructors.
seaCtor
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())

seaCtor eTree
 = do
	let eExpanded	= addSuperProtosTree
			$ expandCtorTree
			$ eTree

	dumpET Arg.DumpSeaCtor "sea-ctor"
		$ eraseAnnotsTree eExpanded

	return eExpanded


-- | Expand code for creating thunks
seaThunking
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())

seaThunking eTree
 = do
	let tree'	= thunkTree eTree
	dumpET Arg.DumpSeaThunk "sea-thunk"
		$ eraseAnnotsTree tree'

	return tree'


-- | Add code for forcing suspensions
seaForce
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())

seaForce eTree
 = do
	let tree'	= forceTree eTree
	dumpET Arg.DumpSeaForce "sea-force"
		$ eraseAnnotsTree tree'

 	return	tree'


-- | Store pointers on GC slot stack.
seaSlot
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()		-- sea tree
	-> Tree ()		-- sea header
	-> C.Glob		-- header glob, used to get arities of supers
	-> C.Glob		-- source glob, TODO: refactor to take from Sea glob
	-> IO (Tree ())

seaSlot	eTree eHeader cgHeader cgSource
 = do
 	let tree'	= slotTree eTree eHeader cgHeader cgSource
	dumpET Arg.DumpSeaSlot "sea-slot"
		$ eraseAnnotsTree tree'

	return	tree'


-- | Flatten out match expressions.
seaFlatten
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String
	-> Tree ()
	-> IO (Tree ())

seaFlatten unique eTree
 = do	let tree'	= flattenTree unique eTree
	dumpET Arg.DumpSeaFlatten "sea-flatten"
		$ eraseAnnotsTree tree'

	return	tree'


-- | Add module initialisation code
seaInit
	:: (?args :: [Arg.Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> ModuleId
	-> (Tree ())
	-> IO (Tree ())

seaInit moduleName eTree
 = do 	let tree'	= initTree moduleName eTree
 	dumpET Arg.DumpSeaInit "sea-init"
		$ eraseAnnotsTree tree'

	return tree'


-- | Create C source files
outSea
	:: (?args :: [Arg.Arg])
	=> ModuleId
	-> (Tree ())		-- sea source
	-> FilePath		-- path of the source file
	-> [FilePath]		-- paths of the imported .h header files
	-> [String]		-- extra header files to include
	-> IO	( String
		, String )

outSea
	moduleName
	eTree
	pathThis
	pathImports
	extraIncludes

 = do
	-- Break up the sea into Header/Code parts.
	let 	([ 	_seaProtos, 		seaSupers
		 , 	_seaCafProtos,		seaCafSlots,		seaCafInits
		 ,      _seaData
		 , 	_seaPCtorTag ], junk)

		 = partitionFs
			[ (=@=) PProto{}, 	(=@=) PSuper{}
			, (=@=) PCafProto{},	(=@=) PCafSlot{},	(=@=) PCafInit{}
			, (=@=) PData{}
			, (=@=) PCtorTag{} ]
			eTree

	when (not $ null junk)
	 $ panic "Main.Sea" $ "junk sea bits = " ++ show junk ++ "\n"

	-- Build the C header
	let seaHeaderS
		= makeSeaHeader eTree pathThis pathImports extraIncludes

	-- Build the C code
	let seaIncSelf	= modIncludeSelf pathThis

	let seaCode
		=  makeComments pathThis
		++ [seaIncSelf]
		++ [PBlank]	++ seaCafSlots
		++ [PBlank]	++ seaCafInits
		++ [PBlank]	++ seaSupers

	let seaCodeS
		= catMap pprStrPlain
			$ eraseAnnotsTree seaCode

	--
	return	( seaHeaderS
		, seaCodeS )

modIncludeSelf p
 = let 	Just name	= takeLast $ chopOnRight '/'
 			$ nameTItoH p
   in	PIncludeAbs $ name

modIncludes pathImports
  = map PInclude pathImports

nameTItoH nameTI
 = let	parts	= chopOnRight '.' nameTI
   in   concat (init parts ++ ["ddc.h"])

makeComments pathThis
  = map PComment
	[ "-----------------------"
	, "      source: " ++ pathThis
	, "generated by: " ++ Version.ddcName
	, "" ]

makeIncludeDefTag pathThis
 = filter (\c -> isAlpha c || isDigit c)
 	$ pathThis


-- | Create the C header file for this module.
makeSeaHeader :: (Tree ()) -> String -> [String] -> [String] -> String
makeSeaHeader eTree pathThis pathImports extraIncludes
 = do		-- Break up the sea into Header/Code parts.
	let 	([ seaProtos, seaCafProtos, seaCtorTag ], _junk)
		 = partitionFs
			[ (=@=) PProto{}, (=@=) PCafProto{}, (=@=) PCtorTag{} ]
			eTree
	let defTag	= makeIncludeDefTag pathThis
	let hdr
		= makeComments pathThis
		++	[ PHackery ("#ifndef _inc" ++ defTag ++ "\n")
			, PHackery ("#define _inc" ++ defTag ++ "\n\n")
			, PInclude "runtime/Runtime.h"
			, PInclude "runtime/Prim.h" ]

		++ modIncludes pathImports
		++ (map PInclude extraIncludes)

		++ [ PBlank ]	++ seaCtorTag
		++ [ PBlank ]	++ seaCafProtos
		++ [ PBlank ]	++ seaProtos
		++ [ PHackery "\n#endif\n\n" ]
	catMap pprStrPlain $ eraseAnnotsTree hdr


-- | Add main module entry point code.
seaMain	:: (?args :: [Arg.Arg])
	=> [ModuleId]
	-> ModuleId
	-> IO (Tree ())

seaMain imports mainModule
	= return $ mainTree mainModule imports
			(not $ elem Arg.NoImplicitHandler ?args)





