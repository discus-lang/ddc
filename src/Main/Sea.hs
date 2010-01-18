
module Main.Sea
	( seaSub
	, seaCtor
	, seaThunking
	, seaForce
	, seaSlot
	, seaFlatten
	, seaInit
	, seaMain

	, outSea)
where

-----
import Shared.Var		(Module(..))
import Shared.Error
import Shared.Pretty

import Sea.Pretty
import Sea.Exp
import Sea.Util

import Sea.Sub			(subTree)
import Sea.Ctor			(expandCtorTree)
import Sea.Proto		(addSuperProtosTree)
import Sea.Thunk		(thunkTree)
import Sea.Force		(forceTree)
import Sea.Slot			(slotTree)
import Sea.Flatten		(flattenTree)
import Sea.Init			(initTree, mainTree)

import Sea.Plate.Trans

import qualified Main.Version	as Version
import Main.Arg
import Main.Dump

import System.Cmd
import System.Exit
import qualified Data.Map	as Map
import Data.Map			(Map)
import Data.Char

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

------
-- seaSub
--
seaSub
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())
	
seaSub tree
 = do
 	let tree'
		= subTree tree
		
	dumpET DumpSeaSub "sea-sub" 
		$ eraseAnnotsTree tree'
	
	return tree'
	
		
-----------------------
-- expandCtor
--
seaCtor 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())

seaCtor
	eTree
 = do
	let eExpanded	= addSuperProtosTree
			$ expandCtorTree
			$ eTree
	
	dumpET DumpSeaCtor "sea-ctor" 
		$ eraseAnnotsTree eExpanded
	
	return eExpanded
		

-----------------------
-- expandThunking
--
seaThunking 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())
	
seaThunking	 
	eTree
 = do
	let tree'	= thunkTree eTree
	dumpET DumpSeaThunk "sea-thunk" 
		$ eraseAnnotsTree tree'
	
	return tree'


-----------------------
-- seaForce
--
seaForce 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()
	-> IO (Tree ())
	
seaForce
	eTree
 = do
	let tree'	= forceTree eTree
	dumpET DumpSeaForce "sea-force" 
		$ eraseAnnotsTree tree'

 	return	tree'


-----------------------
-- seaSlot
--
seaSlot
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Tree ()		-- sea tree
	-> Tree ()		-- sea header
	-> Set Var		-- CAF vars
	-> IO (Tree ())
	
seaSlot	eTree eHeader cafVars
 = do
 	let tree'	= slotTree eTree eHeader cafVars
	dumpET DumpSeaSlot "sea-slot" 
		$ eraseAnnotsTree tree'
	
	return	tree'


-----------------------
-- seaFlatten
--
seaFlatten
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String
	-> Tree ()
	-> IO (Tree ())
	
seaFlatten unique eTree
 = do	let tree'	= flattenTree unique eTree
	dumpET DumpSeaFlatten "sea-flatten" 
		$ eraseAnnotsTree tree'
	
	return	tree'
 

-----------------------
-- seaInit
--
seaInit
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Module
	-> (Tree ())
	-> IO (Tree ())
	
seaInit moduleName eTree
 = do 	let tree'	= initTree moduleName eTree
 	dumpET DumpSeaInit "sea-init"
		$ eraseAnnotsTree tree'
		
	return tree'
	
-----------------------
-- outSea
--		
outSea 
	:: (?args :: [Arg])
	=> Module
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
	let 	([ 	seaProtos, 		seaSupers
		 , 	seaCafProtos,		seaCafSlots,		seaCafInits
		 , 	seaAtomProtos,		seaAtoms
		 , 	seaStructs
		 , 	seaHashDefs ], [])

		 = partitionFs
			[ (=@=) PProto{}, 	(=@=) PSuper{}
			, (=@=) PCafProto{},	(=@=) PCafSlot{},	(=@=) PCafInit{}
			, (=@=) PAtomProto{},  	(=@=) PAtom{}
			, (=@=) PStruct{}
			, (=@=) PHashDef{} ]
			eTree
			
	
	-- Build the C header
	let defTag	= makeIncludeDefTag pathThis
	let seaHeader
		=  [ PHackery $ makeComments pathThis
		   , PHackery ("#ifndef _inc" ++ defTag ++ "\n")
		   , PHackery ("#define _inc" ++ defTag ++ "\n\n") 
		   , PInclude "runtime/Runtime.h"
		   , PInclude "runtime/Prim.h" ]
		++ modIncludes pathImports
		++ (map PInclude extraIncludes)

		++ [ PHackery "\n"]	++ seaHashDefs
		++ [ PHackery "\n"]	++ seaStructs
		++ [ PHackery "\n"]	++ seaAtomProtos
		++ [ PHackery "\n"]	++ seaCafProtos
		++ [ PHackery "\n"]	++ seaProtos
		++ [ PHackery "\n#endif\n\n" ]

	let seaHeaderS	
		= catMap pprStrPlain 
			$ eraseAnnotsTree seaHeader

	-- Build the C code
	let seaIncSelf	= modIncludeSelf pathThis
	
	let seaCode
		=  [PHackery $ makeComments pathThis]
		++ [seaIncSelf]
		++ [PHackery "\n"]	++ seaAtoms
		++ [PHackery "\n"]	++ seaCafSlots
		++ [PHackery "\n"]	++ seaCafInits
		++ [PHackery "\n"]	++ seaSupers

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

--  = 	map (\p -> PInclude $ nameTItoH p) pathImports

nameTItoH nameTI
 = let	parts	= chopOnRight '.' nameTI
   in   concat (init parts ++ ["ddc.h"])
   
makeComments pathThis
  = unlines
	[ "// -----------------------"
	, "//       source: " ++ pathThis
	, "// generated by: " ++ Version.ddcName
	, "" ]
	
makeIncludeDefTag pathThis
 = filter (\c -> isAlpha c || isDigit c)
 	$ pathThis
		
	


-----------------------
-- seaMain
--
seaMain	:: (?args :: [Arg])
	=> [Module]
	-> Module
	-> IO (Tree ())
	
seaMain imports mainModule
	= return $ mainTree imports mainModule
	




