
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
  	 $ dumpFile
		(fileBase ++ ".dump-" ++ name ++ ".dc")
		(pprStr pprMode (punc (semi % nlnl % nl) (C.treeOfGlob glob) % nl))

	return ()


-- | Dump a core tree
dumpCT flag name sourceTree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 $ dumpFile
		(?pathSourceBase ++ ".dump-" ++ name ++ ".dc")
		(pprStr pprMode (punc (semi % nlnl % nl) sourceTree % nl))

	return ()


-- | Dump a source tree
dumpST flag name sourceTree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args
 	when (elem flag ?args)
  	 $ dumpFile
		(?pathSourceBase ++ ".dump-" ++ name ++ ".ds")
		(pprStr pprMode	$ vsep $ map ppr sourceTree)

	return ()


-- | Dump a string
dumpS flag name str
 = do	when (elem flag ?args)
	 (dumpFile
	 	(?pathSourceBase ++ ".dump-" ++ name)
		str)

	return ()

-- | Dump a dot file
dumpDot flag name str
 = do	when (elem flag ?args)
	 (dumpFile
	 	(?pathSourceBase ++ ".graph-" ++ name ++ ".dot")
		str)

	return ()




-- Dump a sea tree
dumpET flag name tree
 = do	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args

 	when (elem flag ?args)
  	 (dumpFile
		(?pathSourceBase ++ ".dump-" ++ name ++ ".c")
		(catInt "\n"
			$ map (pprStr pprMode)
			$ tree))

	return ()


dumpFile fname text
 = do	putStrLn $ "  * Dumping: " ++ fname
	writeFile fname text

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
