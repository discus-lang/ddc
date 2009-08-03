
module Main.Source
	( Main.Source.parse
	, sourceSlurpFixTable
	, sourceSlurpInlineVars
	, defix
	, rename
	, sourceKinds
	, desugar
	, alias)

where

import qualified Desugar.Plate.Trans		as D
import qualified Desugar.Exp			as D

import Source.Lexer				(scanModuleWithOffside)
import Source.Parser.Module			(parseModule)
import Source.Slurp				(slurpFixTable, slurpKinds)
import Source.Defix				(defixP)
import Source.Desugar				(rewriteTree)
import Source.Alias				(aliasTree)
import Source.Exp
import Source.TokenShow
import Source.Error
import qualified Source.Token			as Token
import qualified Source.Rename			as S
import qualified Source.RenameM			as S
import qualified Source.RenameM			as Rename


import Main.Arg
import Main.Dump

import qualified Shared.Var		as Var
import Shared.Var			(Var, Module)
import Shared.Pretty
import Shared.Error
import Shared.Base
import Shared.Literal

import qualified Data.Map		as Map
import Data.Map (Map)

import qualified Data.Set		as Set
import Data.Set	(Set)

import System.Exit
import System.IO

import Util

---------------------------------------------------------------------------------------------------
-- | Parse source code.
parse 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> FilePath			-- path of source file
	-> String			-- source of root module
	-> IO 	( Tree SourcePos	-- source parse tree
		, [String])		-- pragma strings
			
parse	fileName
	source
 = do	
	-- lex the source file
	let (toksSource, toksPragma)
		= scanModuleWithOffside source

	dumpS DumpSourceTokens "source-tokens" 
		$ show toksSource

	-- check for strings with tabs in them. We'll reject these.
	let tokStringTabs
		= filter Token.tokenPHasStringTabs toksSource
	
	case tokStringTabs of
	 (t:_) -> exitWithUserError ?args [ErrorLexicalStringTabs t]	

	 [] -> do
		-- expand out \n \b and friends to their literal equivalents
		let expandEscapes tp
			| Token.TokenP {Token.token = t}		<- tp
			, Token.Literal (LiteralFmt (LString s) fmt) 	<- t
			= case Token.expandEscapedChars s of
				Nothing	-> exitWithUserError ?args [ErrorLexicalEscape tp]
				Just s'	-> return $ tp { Token.token = Token.Literal (LiteralFmt (LString s') fmt) }
			
			| otherwise
			= return tp

		toksEscape	<- mapM expandEscapes toksSource
	
		-- set the filename on all the tokens to the current one
		let toksSource'	= map (Token.tokenPSetFileName fileName)    toksEscape

		-- parse the source file
		let sParsed	= parseModule fileName toksSource'
		dumpST 	DumpSourceParse "source-parse" sParsed

		return	( sParsed
			, [str	| Token.CommentPragma str	<- map Token.token toksPragma ])


---------------------------------------------------------------------------------------------------
-- | Slurp out fixity table
sourceSlurpFixTable
	:: Tree SourcePos		-- source and header parse tree
	-> IO [FixDef SourcePos]	-- fixity table
		
sourceSlurpFixTable
	sTree
 = do
	let fixTable	= slurpFixTable sTree
	return	fixTable


-- | Slurp out table of bindings to inline
sourceSlurpInlineVars
	:: Tree SourcePos
	-> IO [Var]
	
sourceSlurpInlineVars 
	sTree
 = 
 	return	$ catMap
			(\p -> case p of
 				PPragma _ [XVar sp v, XList _ xs]
				 | Var.name v == "Inline"
				 , vs		 <- map (\(XVar sp v) -> v) xs
				 -> vs
			 
				_ -> [])
			sTree


---------------------------------------------------------------------------------------------------
-- | Write uses of infix operatiors to preix form.
defix	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree	SourcePos		-- source parse tree
	-> [FixDef SourcePos]		-- fixity table
	-> IO (Tree SourcePos)		-- defixed parse tree, will have no more XInfix nodes.
	
defix	sParsed
	fixTable
 = do
 	let (sDefixed, errss)
			= unzip
			$ map (defixP fixTable)
			$ sParsed

	let errs	= concat errss
	
	when (not $ null errs)
	 $ exitWithUserError ?args errs
	
	-- dump
	dumpST	DumpSourceDefix "source-defix" sDefixed
	
	return	 sDefixed
	

-- | Check scoping of variables and 
-- NOTE:
-- 	We need to rename infix defs _after_ foreign imports
--	We do this so that the Sea name for functions like (+ / primInt32Add)
--	which is present on foreign decls gets propagated to uses of these functions.
--
rename	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> 	[(Module, Tree SourcePos)]
	-> IO 	[(Module, Tree SourcePos)]


rename	mTrees
 = do
	let (mTrees', state')
		= runState (S.renameTrees mTrees)
		$ S.initRenameS

	-- dump
	let Just sTree	= liftM snd $ takeHead mTrees'

	dumpST 	DumpSourceRename "source-rename" sTree
	dumpST 	DumpSourceRename "source-rename--header" (concat $ map snd $ tail mTrees')

	-- exit after dumping, so we can see what's going on.
	when (not $ null $ S.stateErrors state')
	 $ exitWithUserError ?args $ S.stateErrors state'
	
	return mTrees'


-- Slurp out the kinds for user defined classes.
sourceKinds
	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree SourcePos
	-> IO [(Var, Kind)]
	
sourceKinds sTree
 = do	let kinds	= slurpKinds sTree
--	dumpS DumpSourceKinds "source-kinds" 
--		(catMap (\(v, k) -> pprStr $ v %>> " :: " % k % ";\n") kinds)
		
	return	kinds


-----------------------
-- alias
--
alias 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree SourcePos
	-> IO (Tree SourcePos)
	
alias sTree
 = do
 	let sTree'	= aliasTree sTree
--	dumpST	DumpSourceAlias "source-alias" sTree'

	return	sTree'
	
	
-- | Convert from Source to Desugared IR.
desugar
	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> String			-- unique
	-> [(Var, Kind)]		-- kind table
	-> Tree	SourcePos		-- header tree
	-> Tree	SourcePos		-- source tree
	-> IO 	( D.Tree SourcePos
		, D.Tree SourcePos)
	
desugar unique kinds hTree sTree
 = do
	let kindMap	= Map.fromList kinds
	let (hTree', sTree', errors)	
			= rewriteTree unique kindMap hTree sTree
			
	-- dump
	dumpST DumpDesugar "desugar--header" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) hTree')

	dumpST DumpDesugar "desugar--source" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sTree')

	when (not $ null errors)
	 $ exitWithUserError ?args errors
		
	return	(hTree', sTree')


	
