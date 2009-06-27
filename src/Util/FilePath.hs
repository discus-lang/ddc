
module Util.FilePath 
	(baseNameOfPath)
where

import Util.Data.List


-- | Gives the base name of a file path
--	For example:   	baseName "/some/dir/File"	=> "/some/dir/File"
--			baseName "/some/dir/File.o"     => "/some/dir/File"
--			baseName "/some/dir/File.o.out" => "/some/dir/File.o"
--
baseNameOfPath :: FilePath -> FilePath
baseNameOfPath path
 = let	dirParts	= chopOnRight '/' path
	dir		= concat $ init dirParts

 	fileParts	= chopOnLeft '.' $ last dirParts
	file		= case fileParts of
				[p]	-> p
				_	-> concat $ init fileParts
	
   in	dir ++ file
