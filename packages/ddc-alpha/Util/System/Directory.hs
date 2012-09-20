
module Util.System.Directory 
	(findFileInDirs)
where

import System.Directory

-- | Look for a file starting from one of these directories.
findFileInDirs
	:: [FilePath] 			-- import dirs
	-> FilePath			-- name of the file to find
	-> IO (Maybe 
		( FilePath		-- the dir we found the file in
		, FilePath))		-- the full path to the file

findFileInDirs [] name		= return Nothing
findFileInDirs (dir : ds) name
 = do	let testName	= dir ++ "/" ++ name
	exists	<- doesFileExist testName
	if exists
	 then	return $ Just (dir, testName)
	 else	findFileInDirs ds name