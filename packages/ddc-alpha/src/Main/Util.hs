module Main.Util
	(compileExit, outVerb, readUtf8File)
where

import Control.Monad
import DDC.Main.Pretty
import System.Exit
import System.IO


compileExit :: IO a
compileExit = exitWith ExitSuccess


outVerb :: (?verbose :: Bool) => Str -> IO ()
outVerb ss	= when ?verbose (putStr $ pprStrPlain ss)

readUtf8File :: FilePath -> IO String
readUtf8File filePath
 = do	h <- openFile filePath ReadMode
	hSetEncoding h utf8
	hSetEncoding stdout utf8
	hGetContents h

