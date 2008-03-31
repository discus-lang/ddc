module Source.Parser.Module
	( run
	, pModule)

where
import Source.Parser.Pattern

import Source.Lexer
import Source.Exp
import Source.Parser.Base
import qualified Source.Token	as K

import qualified Text.ParserCombinators.Parsec.Prim		as Parsec
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Pos		as Parsec

import Util

-----
run parser str
 = let 	tokens		= scan str
 	eExp		= Parsec.runParser parser () "file" tokens
   in	case eExp of
		Right exp	-> exp
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


-- Expressions -------------------------------------------------------------------------------------

-- | Parse an expression
pExp :: Parser (Exp SP)
pExp
 =    	do 	exps@(x1 : _)	<- Parsec.many1 pExp1
		return	$ XDefixApps (spX x1) exps


-- | Parse an expression that can be used in an application list
pExp1 :: Parser (Exp SP)
pExp1
 =	-- VAR/CON
	do	var	<- pVarCon
		return	$ XVar (spV var) (vNameV var)
  <|>
	-- SYM
	do	sym	<- pSymbol
		return	$ XOp (spV sym) (vNameV sym)

	-- ( EXP )
  <|>	do 	exp	<- pRParen pExp
		return 	exp

	-- if EXP then EXP else EXP
  <|>	do	tok	<- pTok K.If
		exp1	<- pExp
		pTok K.Then
		exp2	<- pExp
		pTok K.Else
		exp3	<- pExp
		return	$ XIfThenElse (spTP tok) exp1 exp2 exp3
	



