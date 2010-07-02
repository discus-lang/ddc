{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for compiler stages dealing with LLVM code.
module Main.Llvm
	(compileViaLlvm)
where

-- main stages
import Main.Setup

import DDC.Var

import qualified Source.Pragma		as Pragma
import qualified Module.Scrape		as M
import qualified DDC.Core.Glob		as C
import qualified DDC.Config.Version	as Version

import Llvm
import Llvm.Invoke
-- import Llvm.GhcReplace.Unique

import Util


compileViaLlvm
	:: (?verbose :: Bool, ?pathSourceBase :: FilePath)
	=> Setup			-- ^ Compile setup.
	-> ModuleId			-- ^ Module to compile, must also be in the scrape graph.
	-> FilePath			-- ^ FilePath of source file.
	-> C.Glob			-- ^ Glob of headers.
	-> C.Glob			-- ^ Glob of module itself.
	-> [FilePath]			-- ^ C import directories.
	-> Map ModuleId [a]		-- ^ Module import map.
	-> Bool				-- ^ Module defines 'main' function.
	-> [Pragma.Pragma]		-- ^ Compiler pragmas.
	-> M.Scrape			-- ^ ScrapeGraph of this Module.
	-> Map ModuleId M.Scrape	-- ^ Scrape graph of all modules reachable from the root.
	-> Bool				-- ^ Whether to treat a 'main' function defined by this module
					--	as the program entry point.
	-> IO Bool

compileViaLlvm
	setup modName pathSource cgHeader cgModule importDirs importsExp
	modDefinesMainFn pragmas sRoot scrapes_noRoot blessMain
 = do
	let ?args		= setupArgs setup
	let outfile = ?pathSourceBase ++ ".ddc.ll"
	writeFile outfile $ ppLlvmModule $ ddcModule ?pathSourceBase

	invokeLlvmCompiler ?args ?pathSourceBase []
	invokeLlvmAssembler ?args ?pathSourceBase []

	putStrLn $ "modDefinesMainFn : " ++ (show modDefinesMainFn)

	return modDefinesMainFn


ddcModule :: String -> LlvmModule
ddcModule pathSourceBase
 =	let comments =
 		[ "Generator: " ++ Version.ddcName
		, "Source: " ++ pathSourceBase ++ ".ds"
		, "" ]
	in LlvmModule comments [] [] []
