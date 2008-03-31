module Source.Parser.Module
	( run
	, pModule)

where
import Source.Parser.Pattern
import Source.Parser.Exp
import Source.Parser.Base

import Source.Lexer
import Source.Exp
import qualified Source.Token	as K

import Shared.Pretty

import qualified Text.ParserCombinators.Parsec.Prim		as Parsec
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Pos		as Parsec

import Util

-----
run parser str
 = let 	tokens		= scan str
 	eExp		= Parsec.runParser parser () "file" tokens
   in	case eExp of
		Right exp	-> putStr $ pprStrPlain exp
		Left err	-> error $ show err

-- a whole module		
pModule :: Parser [Top SP]
pModule
 = do	bind	<- pCParen pBind
	return	[PStmt bind]



-- binding
pBind :: Parser (Stmt SP)
pBind
 = do	var		<- pVar
	ws		<- Parsec.many pPat1
	pTok K.Equals
	exp		<- pExp
	return	$ SBindPats (spV var) (vNameV var) ws exp


	



