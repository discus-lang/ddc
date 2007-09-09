
module Main.Path
(
	Path(..),
	makePaths
)

where

import Shared.Error


-----
stage	= "Main.Path"

-----
data	Path
	= Path
	{ pathBase		:: FilePath
	, pathO			:: FilePath
	, pathDI		:: FilePath
	, pathC			:: FilePath
	, pathH			:: FilePath }
	deriving (Show, Eq)
	
	
makePaths ::	FilePath -> Path
makePaths	basePath
 	= Path
	{ pathBase		= basePath
	, pathO			= basePath ++ ".o"
	, pathDI		= basePath ++ ".di"
	, pathC			= basePath ++ ".ddc.c"
	, pathH			= basePath ++ ".ddc.h" }


	
-- define this so we can derive Ord Main.Args
instance Ord Path where
 compare x1 x2	= panic stage 
 		$ "can't compare Path tables."
