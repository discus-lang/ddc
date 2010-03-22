
module Main.Dump
	( dumpST
	, dumpS
	, dumpDot
	, dumpCT
	, dumpET
	, dumpOpen)
where
import System.IO
import Util
import DDC.Main.Arg
import Shared.Pretty		()


-- | Dump a source tree
dumpST flag name sourceTree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 (writeFile 
		(?pathSourceBase ++ ".dump-" ++ name ++ ".ds")
		(concat $ map (pprStr pprMode)
			$ sourceTree))
	
	return ()

-- | Dump a string
dumpS flag name str
 = do	when (elem flag ?args)
	 (writeFile 
	 	(?pathSourceBase ++ ".dump-" ++ name ++ ".ds")
		str)
	
	return ()

-- | Dump a dot file
dumpDot flag name str
 = do	when (elem flag ?args)
	 (writeFile 
	 	(?pathSourceBase ++ ".graph-" ++ name ++ ".dot")
		str)
	
	return ()


-- | Dump a core tree
dumpCT flag name tree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 (writeFile 
		(?pathSourceBase ++ ".dump-" ++ name ++ ".dc")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ tree))
	
	return ()


-- Dump a sea tree
dumpET flag name tree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 (writeFile 
		(?pathSourceBase ++ ".dump-" ++ name ++ ".c")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ tree))
	
	return ()


-----
dumpOpen flag name
 = do	
	if elem flag ?args
	 then do
	 	h	<- openFile 
				(?pathSourceBase ++ ".dump-" ++ name ++ ".ds") 
				WriteMode
		return $ Just h
		
	 else	return Nothing
