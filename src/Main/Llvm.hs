{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for compiler stages dealing with LLVM code.
module Main.Llvm
	(compileViaLlvm)
where

-- main stages
import Main.Setup
import Main.Sea
import Main.Util

import DDC.Var

import qualified Module.Scrape		as M
import qualified DDC.Config.Version	as Version

import Llvm
import Llvm.Invoke

import Sea.Exp

import Util
import qualified Data.Map		as Map

compileViaLlvm
	:: (?verbose :: Bool, ?pathSourceBase :: FilePath)
	=> Setup			-- ^ Compile setup.
	-> ModuleId			-- ^ Module to compile, must also be in the scrape graph.
	-> Tree ()			-- ^ The Tree for the module.
	-> FilePath			-- ^ FilePath of source file.
	-> [FilePath]			-- ^ C import directories.
	-> [FilePath]			-- ^ C include files.
	-> Map ModuleId [a]		-- ^ Module import map.
	-> Bool				-- ^ Module defines 'main' function.
	-> M.Scrape			-- ^ ScrapeGraph of this Module.
	-> Map ModuleId M.Scrape	-- ^ Scrape graph of all modules reachable from the root.
	-> Bool				-- ^ Whether to treat a 'main' function defined by this module
					--	as the program entry point.
	-> IO Bool

compileViaLlvm
	setup modName eInit pathSource importDirs includeFilesHere importsExp
	modDefinesMainFn sRoot scrapes_noRoot blessMain
 = do
	let ?args		= setupArgs setup

	outVerb $ ppr $ "  * Write C header\n"
	writeFile (?pathSourceBase ++ ".ddc.h")
		$ makeSeaHeader
			eInit
			pathSource
			(map ((\(Just f) -> f) . M.scrapePathHeader)
					$ Map.elems scrapes_noRoot)
			includeFilesHere

	outVerb $ ppr $ "  * Generatring LLVM IR code\n"
	writeFile (?pathSourceBase ++ ".ddc.ll")
		$ ppLlvmModule $ ddcModule ?pathSourceBase

	invokeLlvmCompiler ?pathSourceBase []
	invokeLlvmAssembler ?pathSourceBase []

	return modDefinesMainFn


ddcModule :: String -> LlvmModule
ddcModule pathSourceBase
 =	let comments =
 		[ "Generator: " ++ Version.ddcName
		, "Source: " ++ pathSourceBase ++ ".ds"
		, "" ]
	in LlvmModule comments [] [] []
