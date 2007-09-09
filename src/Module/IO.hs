
module Module.IO
(
	munchFileName,
	chopOffExt,
)

where

-----
import qualified System

-----
import Util.List
import Util.Monad

-----
import qualified Shared.Error		as Error

import qualified Source.Lexer		as S
import qualified Source.Parser		as S
import qualified Source.Slurp		as S
import qualified Source.Token		as Token

-----
stage	= "Main.Interface"


-----
-- munchFileName
--	| Break a fileName into directory, base name and extension parts.
--	  eg 
--		muchFileName "path1/path2/path3/file1.file2.ext:
--			=> ("path1/path2/path3", "file1.file2", "ext")
--
munchFileName 
	:: FilePath	
	-> Maybe (FilePath, FilePath, FilePath)

munchFileName	 filePath
 = let 	fileDirParts	= breakOns '/' filePath
 	fileName	= last fileDirParts
	fileNameParts	= breakOns '.' fileName
   in 	
   	if length fileNameParts < 2
	 then Nothing
	 else 
	  let	fileDir		= catInt "/" $ init fileDirParts
		fileBase	= catInt "." $ take (length fileNameParts - 1) fileNameParts
	  	fileExt		= last fileNameParts
	  in	Just (fileDir, fileBase, fileExt)

chopOffExt fileName
 = case init (breakOns '.' fileName) of
 	[]	-> Nothing
	xx	-> Just (catInt "." xx)

-----
