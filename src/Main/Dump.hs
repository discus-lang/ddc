
module Main.Dump
	( dumpST
	, dumpS
	, dumpDot
	, dumpCT
	, dumpET
	, dumpOpen)

where

import Main.Arg
import Main.Path

import qualified Source.Pretty

import qualified Core.Pretty
import qualified Core.Util

import qualified Sea.Pretty
import qualified Type.Pretty

import Shared.Pretty
import System.IO
import Util

-- | Convert an arg into the pretty mode it enables
takePrettyMode :: Arg -> Maybe PrettyMode
takePrettyMode aa
 = case aa of
 	DumpPrettyUnique	-> Just $ PrettyUnique
	DumpPrettyTypeSpaces	-> Just $ PrettyTypeSpaces
	DumpPrettyCoreTypes	-> Just $ PrettyCoreTypes
	_			-> Nothing

-- | Dump a source tree
dumpST flag name sourceTree
 = do
	let [ArgPath paths]	
		= filter (=@= ArgPath{}) ?args

	let pprMode	= catMaybes $ map takePrettyMode ?args

 	when (elem flag ?args)
  	 (writeFile 
		(pathBase paths ++ ".dump-" ++ name ++ ".ds")
		(concat $ map (pprStr pprMode)
			$ sourceTree))
	
	return ()

-- | Dump a string
dumpS flag name str
 = do
 	let [ArgPath paths]
		= filter (=@= ArgPath{}) ?args
		
	when (elem flag ?args)
	 (writeFile 
	 	(pathBase paths ++ ".dump-" ++ name ++ ".ds")
		str)
	
	return ()

-- | Dump a dot file
dumpDot flag name str
 = do
 	let [ArgPath paths]
		= filter (=@= ArgPath{}) ?args
		
	when (elem flag ?args)
	 (writeFile 
	 	(pathBase paths ++ ".graph-" ++ name ++ ".dot")
		str)
	
	return ()


-- | Dump a core tree
dumpCT flag name tree
 = do
	let [ArgPath paths]	
		= filter (=@= ArgPath{}) ?args

	let pprMode	= catMaybes $ map takePrettyMode ?args

 	when (elem flag ?args)
  	 (writeFile 
		(pathBase paths ++ ".dump-" ++ name ++ ".dc")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ tree))
	
	return ()


-- Dump a sea tree
dumpET flag name tree
 = do
	let [ArgPath paths]	
		= filter (=@= ArgPath{}) ?args

	let pprMode	= catMaybes $ map takePrettyMode ?args

 	when (elem flag ?args)
  	 (writeFile 
		(pathBase paths ++ ".dump-" ++ name ++ ".c")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ tree))
	
	return ()


-----
dumpOpen flag name
 = do	
 	let [ArgPath paths]	=
		 [x | x@ArgPath{} <- ?args]

	if elem flag ?args
	 then do
	 	h	<- openFile 
				(pathBase paths ++ ".dump-" ++ name ++ ".ds") 
				WriteMode
		return $ Just h
		
	 else	return Nothing
