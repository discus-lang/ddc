
module Source.Parser.Exp
	(pExp, pExp1)

where

import Source.Exp
import Source.Parser.Pattern
import Source.Parser.Base
import qualified Source.Token	as K

import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec


-- Expressions -------------------------------------------------------------------------------------

-- | Parse an expression
pExp :: Parser (Exp SP)
pExp
 =   	-- application
  	do 	exps@(x1 : _)	<- Parsec.many1 pExp2
		return	$ XDefixApps (spX x1) exps

pExp2 :: Parser (Exp SP)
pExp2	
 = 	-- projections
 	-- EXP . EXP   /   EXP # EXP
 	(Parsec.try $ do	
	 	exp	<- chainl1_either pExp1 pProj
 		return	exp)

  <|>	do
  		exp	<- pExp1
		return	exp
 
pProj :: Parser (Exp SP -> Exp SP -> Either String (Exp SP))
pProj 
 = 	do	pTok K.Dot
	 	return	(makeProjV JField JIndex)

 <|>	do	pTok K.Hash
 		return	(makeProjV JFieldR JIndexR)

makeProjV fun funR x y
	| XVar sp v		<- y	= Right $ XProj (spX x) x (fun  sp v)
	| XDefixApps sp _	<- y	= Right $ XProj (spX x) x (funR sp y)
	| otherwise			= Left "pProj: LHS is not a field"


-- | Parse an expression that can be used in an application
pExp1 :: Parser (Exp SP)
pExp1
 =	-- VAR & { TYPE }								-- NOT FINISHED
 	-- overlaps with VAR
	(Parsec.try $ do
		field	<- pVar
		pTok K.And
		var2	<- pCParen pVar
		return	$ XProjT (spV field) undefined (JField (spV field) (vNameF field)))
 		 
 	-- VAR/CON
  <|>	do	var	<- pVarCon
		return	$ XVar (spV var) (vNameV var)

	-- VARFIELD					-- TODO: change this when we move to parsec
  <|>	do	var	<- pVarField
		return	$ XObjField (spV var) (vNameV var)
	
  <|>
	-- SYM
	do	sym	<- pSymbol
		return	$ XOp (spV sym) (vNameV sym)

  <|>	-- ()
  	do	tok	<- pTok K.Unit
		return	$ XUnit (spTP tok)

  <|>	-- CONST
  	do	(const, sp) <- pConstSP
		return	$ XConst sp const

  <|>	-- \. VAR EXP ..
  	-- overlaps with next lambda forms
  	(Parsec.try $ do
		tok	<- pTok K.BackSlashDot
		var	<- pVar
		exps	<- Parsec.many pExp1
		return	$ XLambdaProj (spTP tok) (JField (spTP tok) (vNameF var)) exps)
		
  <|>   -- \ case { ALT .. }
  	-- overlaps with the nexta lambda form
	(Parsec.try $ do
		tok	<- pTok K.BackSlash
		pTok K.Case
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt (pTok K.SemiColon))
		return	$ XLambdaCase (spTP tok) alts)

  <|>	-- \ PAT .. -> EXP
	do	tok1	<- pTok K.BackSlash
		pats	<- Parsec.many1 pPat1
		pTok	<- pTok K.RightArrow
		exp	<- pExp
		return	$ XLambdaPats (spTP tok1) pats exp

  <|>	-- case EXP of { ALT .. }
  	do	tok	<- pTok K.Case
		exp	<- pExp 
		pTok K.Of
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt (pTok K.SemiColon))
		return	$ XCase (spTP tok) exp alts

  <|>	-- match { ALT .. }
  	do	tok	<- pTok K.Match
		alts	<- pCParen (Parsec.sepEndBy1 pMatchAlt (pTok K.SemiColon))
		return	$ XMatch (spTP tok) alts

  <|>	-- do { SIG/STMT/BIND .. }
	do	tok	<- pTok K.Do
		stmts	<- pCParen (Parsec.sepEndBy1 pStmt_sigStmtBind (pTok K.SemiColon))
		return	$ XDo (spTP tok) stmts

  <|>	-- let { SIG/BIND .. } in EXP
  	do	tok	<- pTok K.Let
		binds	<- pCParen (Parsec.sepEndBy1 pStmt_sigBind (pTok K.SemiColon))
		pTok K.In
		exp	<- pExp
		return	$ XLet (spTP tok) binds exp

	-- if EXP then EXP else EXP
  <|>	do	tok	<- pTok K.If
		exp1	<- pExp
		pTok K.Then
		exp2	<- pExp
		pTok K.Else
		exp3	<- pExp
		return	$ XIfThenElse (spTP tok) exp1 exp2 exp3

	-- ( EXP )
  <|>	do 	exp	<- pRParen pExp
		return 	exp

		
-- Alternatives ------------------------------------------------------------------------------------

-- | Parse a case style alternative
pCaseAlt :: Parser (Alt SP)						-- NOT finished, guards in alts | 
pCaseAlt
 = 	-- PAT -> EXP
   do	pat	<- pPat 
 	pTok K.RightArrow
	exp	<- pExp
	return	$ APat (spW pat) pat exp


-- | Parse a match style alternative
pMatchAlt :: Parser (Alt SP)
pMatchAlt
 =	-- | GUARD, ... = EXP
   	do	tok	<- pTok K.Bar
	   	guards	<- (Parsec.sepEndBy1 pGuard (pTok K.Comma))
		pTok K.Equals
		exp	<- pExp 
		return	$ AAlt (spTP tok) guards exp

  <|>	-- \= EXP
  	do	tok	<- pTok K.GuardDefault
		exp	<- pExp
		return	$ ADefault (spTP tok) exp


-- | Parse a guard
pGuard :: Parser (Guard SP)
pGuard	
 = 	-- PAT <- EXP
 	-- overlaps with EXP
	(Parsec.try $ do 
		pat	<- pPat
	 	pTok K.LeftArrow
		exp	<- pExp
		return	$ GExp (spW pat) pat exp)

	-- EXP
 <|>	do	exp	<- pExp
 		return	$ GBool (spX exp) exp


-- Statements --------------------------------------------------------------------------------------

-- | Parse a signature, statement or binding
pStmt_sigStmtBind :: Parser (Stmt SP)
pStmt_sigStmtBind
 = 									-- NOT FINISHED, add sig
	-- bindings overlap with expressions
 	(Parsec.try pStmt_bind)
	
  <|>	do	exp	<- pExp
  		return	$ SStmt (spX exp) exp


-- | Parse a signature or binding
pStmt_sigBind :: Parser (Stmt SP)
pStmt_sigBind
 = 									-- NOT FINISHED, add sig
   do	pStmt_bind


-- | Parse a bind (only)
pStmt_bind :: Parser (Stmt SP)
pStmt_bind
 = do	var	<- pVar
	pats	<- Parsec.many pPat1
	pTok K.Equals
	exp	<- pExp
	return	$ SBindPats (spV var) (vNameV var) pats exp

		
  
		
		


