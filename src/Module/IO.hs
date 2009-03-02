
module Module.IO
	( normaliseFileName
	, munchFileName
	, chopOffExt
	, findFile)
where

import Util
import System
import System.Directory

-- | Normalise a file name
normaliseFileName
	:: FilePath
	-> (String, String, String, String)

normaliseFileName fileName
 = let	fileName'
	 = case fileName of
		('/':_)		-> fileName
		('~':_)		-> fileName
		('.':'/':_)	-> fileName
		_		-> "./" ++ fileName

	-- Break out the file name
	Just (fileDir, fileBase, fileExt)
		= munchFileName fileName'
		
	fileDir'
	 = if fileDir == []
		then "."
		else fileDir

   in	(fileName', fileDir', fileBase, fileExt)


-- | Break a fileName into directory, base name and extension parts.
--
-- > munchFileName "path1/path2/path3/file1.file2.ext"
-- >  => ("path1/path2/path3", "file1.file2", "ext")
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


-- | Chop the file type extension from this file name
chopOffExt :: FilePath -> Maybe String
chopOffExt fileName
 = case init (breakOns '.' fileName) of
 	[]	-> Nothing
	xx	-> Just (catInt "." xx)


-- | Try and find a file starting from one of these import dirs
findFile 
	:: [FilePath] 			-- import dirs
	-> FilePath			-- name of the file to find
	-> IO (Maybe 
		( FilePath		-- the dir we found the file in
		, FilePath))		-- the full path to the file

findFile importDirs name
 	-- out of dirs to seach
	| []		<- importDirs
 	= return Nothing

 	-- search this dir
	| (dir : ds)	<- importDirs
 	= do	let testName	= dir ++ "/" ++ name
		exists	<- doesFileExist testName
		if exists
		 then	return $ Just (dir, testName)
		 else	findFile ds name
