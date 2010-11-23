
module Main.Dump
	( dumpST
	, dumpS
	, dumpDot
	, dumpET
	, dumpOpen
	, dumpCT
	, dumpCG)
where
import System.IO
import Util
import DDC.Main.Arg
import qualified DDC.Core.Glob	as C

-- | Dump a core glob.
dumpCG args fileBase flag name glob
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg args

 	when (elem flag args)
  	 (writeFile 
		(fileBase ++ ".dump-" ++ name ++ ".dc")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ C.treeOfGlob glob))
	
	return ()


-- | Dump a source tree
dumpST flag name sourceTree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args
 	when (elem flag ?args)
  	 $ writeFile 
		(?pathSourceBase ++ ".dump-" ++ name ++ ".ds")
		(pprStr pprMode	$ vsep $ map ppr sourceTree)
	
	return ()

-- | Dump a core
dumpCT flag name sourceTree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 (writeFile 
		(?pathSourceBase ++ ".dump-" ++ name ++ ".dc")
		(pprStr pprMode
			$ vcat
			$ sourceTree))
	
	return ()

-- | Dump a string
dumpS flag name str
 = do	when (elem flag ?args)
	 (writeFile 
	 	(?pathSourceBase ++ ".dump-" ++ name)
		str)
	
	return ()

-- | Dump a dot file
dumpDot flag name str
 = do	when (elem flag ?args)
	 (writeFile 
	 	(?pathSourceBase ++ ".graph-" ++ name ++ ".dot")
		str)
	
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
