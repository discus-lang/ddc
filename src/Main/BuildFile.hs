
-- | Load a build file
--	Build files contain extra information related to actually building the program.
--	Eg what external libraries to link with, and what flags to pass to the C compiler and linker.

module Main.BuildFile
	( Build(..)
	, verbLoadBuildFile
	, loadBuildFile
	, chopSection)
where
import DDC.Main.Error
import System.Directory
import Data.Char
import Util

stage	= "Main.BuildFile"

data Build
	= Build
	{ -- Extra args to use when compiling
	  buildExtraDDCArgs	:: [String]
		
	  -- Extra libraries to link with
	, buildExtraLinkLibs	:: [String]
	
	  -- Extra dirs to look for libraries in
	, buildExtraLinkLibDirs :: [String]
	
	  -- Extra objects to link with
	, buildExtraLinkObjs	:: [String] 
	
	  -- Extra flags to pass to the C compiler
	, buildExtraCCFlags	:: [String]
	
	  -- Extra flags to pass to the linker
	, buildExtraLDFlags	:: [String] 
	
 	  -- Starting heap size for compiled program, or Nothing for default.
	, buildStartHeapSize		:: Maybe Integer
	
	  -- Starting slot stack size of compiled program, or Nothing for default.
	, buildStartSlotStackSize 	:: Maybe Integer
	
	  -- Starting context stack size of compiled program, or Nothing for default.
	, buildStartContextStackSize	:: Maybe Integer }
	
	deriving (Show)


-- | An empty build spec
buildZero
	= Build
	{ buildExtraDDCArgs		= []
	, buildExtraLinkLibs		= []
	, buildExtraLinkLibDirs		= []
	, buildExtraLinkObjs		= []
	, buildExtraCCFlags		= []
	, buildExtraLDFlags		= [] 
	, buildStartHeapSize		= Nothing
	, buildStartSlotStackSize	= Nothing
	, buildStartContextStackSize	= Nothing }

-- | Add the info from two builds together, preferring info from the second.
buildAdd b1 b2
	= Build
	{ buildExtraDDCArgs	= buildExtraDDCArgs  	b1 ++ buildExtraDDCArgs 	b2
	, buildExtraLinkLibs	= buildExtraLinkLibs 	b1 ++ buildExtraLinkLibs	b2
	, buildExtraLinkLibDirs	= buildExtraLinkLibDirs b1 ++ buildExtraLinkLibDirs	b2
	, buildExtraLinkObjs	= buildExtraLinkObjs 	b1 ++ buildExtraLinkObjs	b2
	, buildExtraCCFlags	= buildExtraCCFlags  	b1 ++ buildExtraCCFlags 	b2
	, buildExtraLDFlags	= buildExtraLDFlags  	b1 ++ buildExtraLDFlags 	b2 

	, buildStartHeapSize		
		= takeFirstJust [ buildStartHeapSize b1
				, buildStartHeapSize b2]

	, buildStartSlotStackSize
		= takeFirstJust [ buildStartSlotStackSize b1
				, buildStartSlotStackSize b2]

	, buildStartContextStackSize
		= takeFirstJust [ buildStartContextStackSize b1
				, buildStartContextStackSize b2]
	}
	
	
verbLoadBuildFile :: Bool -> FilePath -> IO (Maybe Build)
verbLoadBuildFile verbose path
 = do	
	when verbose
 	 $	putStr $ "  * Checking for build file " ++ path ++ "\n"
	
	mBuild	<- loadBuildFile path
	
	when verbose
	 $ case mBuild of
	 	Nothing		-> putStr $ "    - none\n\n"
		Just build	-> putStr $ " " ++ show build ++ "\n\n"
	
	return mBuild

-- | Load a build file from this file path
--	If it doesn't exist then return Nothing
loadBuildFile :: FilePath -> IO (Maybe Build)
loadBuildFile pathBuild
 = do	exists	<- doesFileExist pathBuild
 	if exists
	    then do
	    	src		<- readFile pathBuild
		let build	= parseSections pathBuild (lines src) buildZero
		return		$ Just build
		
	    else return		$ Nothing


-- | Parse lines from the build file, adding them to the build info
parseSections :: FilePath -> [String]	-> Build -> Build
parseSections pathBuild [] build = build
parseSections pathBuild ss build
 = let	(build1, ssRest)	 = parseSection pathBuild ss
   in	parseSections pathBuild ssRest (buildAdd build build1) 
   

-- | Parse a section in the build file
parseSection 
	:: FilePath		-- path to the build file
	-> [String] 
	-> ( Build		-- build info from the parsed section
	   , [String])		-- more lines in the file

parseSection pathBuild (s : ss)
	| isNil $ words s
	= parseSection pathBuild ss

	| isPrefixOf "extra-ddc-args:" s
	, (words, ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildExtraDDCArgs  = words }
	  , ssRest)		

	| isPrefixOf "extra-link-libs:" s
	, (words, ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildExtraLinkLibs  = words }
	  , ssRest)		

	| isPrefixOf "extra-link-lib-dirs:" s
	, (words, ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildExtraLinkLibDirs  = words }
	  , ssRest)		

	| isPrefixOf "extra-link-objs:" s
	, (words, ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildExtraLinkObjs  = words }
	  , ssRest)		

	| isPrefixOf "start-heap-size:" s
	, ([str], ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildStartHeapSize	= Just $ read str }
	  , ssRest)

	| isPrefixOf "start-slot-stack-size:" s
	, ([str], ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildStartSlotStackSize	= Just $ read str }
	  , ssRest)

	| isPrefixOf "start-context-stack-size:" s
	, ([str], ssRest)	<- chopSection (s : ss)
	= ( buildZero { buildStartContextStackSize = Just $ read str }
	  , ssRest)

	| otherwise
	= panic stage $ ppr
		[ "Cannot parse build file " % pathBuild 	% "\n"
		, "    unknown section header: '" % s		% "'\n"]


-- | Chop out words from a build file.
--	The section starts after the ':' on the first line and extends until the next line
--	with a non-space charater in the first column.
--
chopSection 
	:: [String]		-- lines of the file
				-- first line should be the   "tag: ... " line from a section header

	-> ( [String]		-- words in the section
	   , [String])		-- lines after this section

chopSection (s : ssMore)
 = let	-- get the rest of the words after the section header
 	(':' : sRest)	= snd $ splitOnLeft ':' s

	-- this section continues until the next line with a character in the first column
	isSectionStart [] 	= False
	isSectionStart (x : _)
		| isSpace x	= False
		| otherwise	= True

 	(ssThis, ssRest)	
		= splitWhenLeft isSectionStart ssMore

  in	( words $ concat $ sRest : ssThis
  	, ssRest)

