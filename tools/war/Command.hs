
-- | IO commands that might fail.
module Command
	( io
	, DirPath
	, IOF
	, IOFail(..)
	, timeIOF_
	, system
	, fileExists
	, dirExists
	, fileIsEmpty
	, move
	, moveIfExists
	, copy
	, remove
	, removeIfExists
	, inDir 
	, lsFilesIn
	, lsDirsIn
	, lsDirs
	, lsFiles
	, chmod)
where
import Timing
import qualified System.Cmd
import System.Exit
import System.Directory
import System.Time
import Control.Monad.Error
import Data.List

type DirPath	= FilePath
io x		= liftIO x

-- Actions -------------------------------------------------------------------
-- An IO action that might fail
type IOF a = ErrorT IOFail IO a

data IOFail
	= IOFailOther 	  	String
	| IOFailCommand 	String
	| IOFailRemoveNotExist	FilePath
	deriving (Eq, Show)

instance Error IOFail where
 strMsg s	= IOFailOther s


timeIOF_ :: IOF a -> IOF TimeDiff
timeIOF_ iof
 = do	let ioAction	= runErrorT iof 
	(result, time)	<- liftIO $ timeIO ioAction

	case result of
	 Left err	-> throwError err
	 Right x	-> return time


-- Commands -------------------------------------------------------------------
-- | Run an external command
system :: String -> ErrorT IOFail IO ()
system cmdString
 = do	exitCode	<- liftIO $ System.Cmd.system cmdString
	case exitCode of
	 ExitSuccess	-> return ()
	 ExitFailure _	-> throwError (IOFailCommand cmdString)

-- Existance ------------------------------------------------------------------
fileExists :: FilePath -> IOF Bool
fileExists path	
	= liftIO $ doesFileExist path

dirExists :: FilePath -> IOF Bool
dirExists path	
	= liftIO $ doesDirectoryExist path


-- Emptiness ------------------------------------------------------------------
fileIsEmpty :: FilePath -> IOF Bool
fileIsEmpty file
 = do	contents	<- liftIO $ readFile file
	return (null contents)


-- Moving files ---------------------------------------------------------------
move :: FilePath -> FilePath -> IOF ()
move src dst 
	= system $ "mv " ++ src ++ " " ++ dst

moveIfExists :: FilePath -> FilePath -> IOF ()
moveIfExists src dst
 = do	existsFile	<- fileExists src
	existsDir	<- dirExists src
	when (existsFile || existsDir)
	 $ move src dst


-- Copying Files --------------------------------------------------------------
copy :: FilePath -> FilePath -> IOF ()
copy src dst 
	= liftIO $ copyFile src dst


-- Removing files -------------------------------------------------------------
remove :: FilePath -> IOF ()
remove file
 = do	exists	<- fileExists file
	if exists
	 then	liftIO $ removeFile file
	 else	throwError (IOFailRemoveNotExist file)

removeIfExists :: FilePath -> IOF ()
removeIfExists file
 = do	exists	<- fileExists file
	if exists
	 then	liftIO $ removeFile file
	 else	return ()


-- Moving around the file system ----------------------------------------------
-- | Run this command in the given directory
inDir :: DirPath -> IOF a -> IOF a
inDir dir action
 = do	oldDir	<- liftIO $ getCurrentDirectory
	liftIO $ setCurrentDirectory dir

	result	<- action

	liftIO $ setCurrentDirectory oldDir
	return result


-- Listing -------------------------------------------------------------------
-- | Get the names of all files in this directory.
lsFilesIn :: MonadIO m => String -> m [String]
lsFilesIn path
 = do
 	contents	<- liftIO $ getDirectoryContents path
	
	-- filter out directories
	files	<- filterM (\p -> liftM not $ liftIO $ doesDirectoryExist p) 
		$ map (\f -> path ++ "/" ++ f)
		$ dropDotPaths contents

	return	$ sort files

-- | Get the names of all the dirs in this one
lsDirsIn :: MonadIO m => String	 -> m [String]
lsDirsIn path
 = do 
 	contents	<- liftIO $ getDirectoryContents path
	
	-- only keep directories
	dirs	<- filterM (liftIO . doesDirectoryExist)
		$  map (\f -> path ++ "/" ++ f)
		$  dropDotPaths contents

	return	$ sort dirs


lsFiles :: MonadIO m => m [String]
lsFiles 
 = do	files	<- lsFilesIn "."
	return	$ map (drop 2) files


lsDirs :: MonadIO m => m [DirPath]
lsDirs
 = do	dirs	<- lsDirsIn "."
	return	$ map (drop 2) dirs


dropDotPaths xx
	= filter (\f -> f /= "." && f /= "..") xx

-- Mode changing --------------------------------------------------------------
chmod :: String -> String -> IOF ()
chmod mode fileName
 = do	system $ "chmod " ++ mode ++ " " ++ fileName

