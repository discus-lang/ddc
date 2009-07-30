
module Util.FilePath 
	( baseNameOfPath
	, normalMunchFilePath
	, munchFileName
	, chopOffExt
	, fileNameOfPath)
where

import Util.Data.List

-- | Gives the base name of a file path
--   For example:
--
-- >	baseName "/some/dir/File"	=> "/some/dir/File"
-- >	baseName "/some/dir/File.o"     => "/some/dir/File"
-- >	baseName "/some/dir/File.o.out" => "/some/dir/File.o"
baseNameOfPath :: FilePath -> FilePath
baseNameOfPath path
 = let	dirParts	= chopOnRight '/' path
	dir		= concat $ init dirParts

 	fileParts	= chopOnLeft '.' $ last dirParts
	file		= case fileParts of
				[p]	-> p
				_	-> concat $ init fileParts
	
   in	dir ++ file


-- | Normalise a file path, then break it up into
--	normalised name
--	directory
--	base file name
--	extension
normalMunchFilePath
	:: FilePath
	-> (String, String, String, String)

normalMunchFilePath fileName
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


-- | Get just the last, filename portion of this path
fileNameOfPath :: FilePath -> FilePath
fileNameOfPath ss	
	= last $ breakOns '/' ss
