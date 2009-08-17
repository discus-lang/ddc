{-# OPTIONS -O2 #-}

module Source.Parser.Exp
	( pExp, pExp1, pExpRHS
	, pStmt, pStmt_bind, pStmt_sig, pStmt_sigBind)
where

import Data.Maybe (isJust)
import Source.Exp
import Source.Parser.Type
import Source.Parser.Pattern
import Source.Parser.Base
import qualified Source.Token	as K

import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var
import Shared.VarSpace		   (NameSpace(..))

import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec

import Control.Monad

-- Expressions -------------------------------------------------------------------------------------

-- | Parse an expression
pExp :: Parser (Exp SP)
pExp
 = 	-- as while / when / unless are known to have two arguments
	--	we can relax the need to wrap the second one in parens

 	-- while ( EXP ) EXP  /  while EXP1 EXP
	do	tok	<- pTok K.While
		exp1	<-	do	g	<- pRParen pExp
			      		return	g
			<|> 	pExp1

		exp2	<- pExp
		return	$ XWhile (spTP tok) exp1 exp2

 <|>	-- when ( EXP ) EXP  /  when EXP1 EXP
	do	tok	<- pTok K.When
		exp1	<-	do	g	<- pRParen pExp
					return	g
			<|>	pExp1

		exp2	<- pExp
		return	$ XWhen (spTP tok) exp1 exp2

 <|>	-- unless ( EXP ) EXP  /  when EXP1 EXP
	do	tok	<- pTok K.Unless
		exp1	<-	do	g 	<- pRParen pExp
					return	g
			<|>	pExp1

		exp2	<- pExp
		return	$ XUnless (spTP tok) exp1 exp2


  	-- application
 <|>  	do 	exps@(x1 : _)	<- Parsec.many1 pExp2
		case exps of
		 [x]	-> return x
		 _	-> return $ XDefix (spX x1) exps
  <?>   "pExp"

pExp2 :: Parser (Exp SP)
pExp2
 = 	-- projections
 	-- EXP . EXP   /   EXP . (EXP)  /  EXP # EXP
 	(Parsec.try $ do
	 	exp	<- chainl1_either pExp1' pProj
 		return	$ stripXParens exp)

  <|>	do	exp	<- pExp1
		return	exp
  <?>   "pExp2"

-- | Parse an expression that can be used in an application
pExp1 :: Parser (Exp SP)
pExp1
 = do	exp	<- pExp1'
 	return	$ stripXParens exp

pExp1'
 =
	-- VAR & { TYPE }								-- NOT FINISHED
 	-- overlaps with VAR
	(Parsec.try $ do
		field	<- liftM vNameF (pQualified pVar)
		pTok K.And
		t	<- pCParen pType_body
		return	$ XProjT (spV field) t (JField (spV field) field))

  <|>	-- VAR/CON
	do	var	<- liftM vNameV (pQualified pVarCon)
		return	$ XVar (spV var) var

  <|>	-- VARFIELD					-- TODO: change this when we move to parsec
	do	var	<- liftM vNameF pVarField
		return	$ XObjField (spV var) var

  <|>	-- SYM
	do	sym	<- liftM vNameV pSymbol
		return	$ XOp (spV sym) sym

  <|>	-- `VAR`
  	do	pTok K.BackTick
		var	<- liftM vNameV (pQualified pVar)
		pTok K.BackTick
		return	$ XOp (spV var) var

  <|>	-- ()
  	do	tok	<- pTok K.Unit
		return	$ XVar (spTP tok) (Var.primUnit)

  <|>	-- lit
  	do	(lit, sp) <- pLiteralFmtSP
		return	$ XLit sp lit

  <|>	-- case EXP of { ALT .. }
  	do	tok	<- pTok K.Case
		exp	<- pExp
		pTok K.Of
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt pSemis)
		return	$ XCase (spTP tok) exp alts

  <|>	-- match { ALT .. }
  	do	tok	<- pTok K.Match
		alts	<- pCParen (Parsec.sepEndBy1 pMatchAlt pSemis)
		return	$ XMatch (spTP tok) alts

  <|>	-- do { SIG/STMT/BIND .. }
	do	tok	<- pTok K.Do
		stmts	<- pCParen (Parsec.sepEndBy1 pStmt pSemis)
		return	$ XDo (spTP tok) stmts

  <|>	-- let { SIG/BIND .. } in EXP
  	do	tok	<- pTok K.Let
		binds	<- pCParen (Parsec.sepEndBy1 pStmt_sigBind pSemis)
		pTok K.In
		exp	<- pExp
		return	$ XLet (spTP tok) binds exp

  <|>	-- if EXP then EXP else EXP
	do	tok	<- pTok K.If
		exp1	<- pExp
		pTok K.Then
		exp2	<- pExp
		pTok K.Else
		exp3	<- pExp
		return	$ XIfThenElse (spTP tok) exp1 exp2 exp3

  <|>	-- \. VAR EXP ..
  	-- overlaps with next lambda forms
  	(Parsec.try $ do
		tok	<- pTok K.BackSlashDot
		var	<- liftM vNameF pVar
		exps	<- Parsec.many pExp1
		return	$ XLambdaProj (spTP tok) (JField (spTP tok) var) exps)

  <|>	-- try EXP catch { ALT .. } (with { STMT; .. })
  	do	tok	<- pTok K.Try
		exp1	<- pExp
		pTok K.Catch
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt pSemis)

		mWith	<-	do	pTok K.With
					stmts	<- pCParen (Parsec.sepEndBy1 pStmt pSemis)
				 	return	$ Just (XDo (spTP tok) stmts)

			<|> 	return Nothing

		return	$ XTry (spTP tok) exp1 alts mWith

  <|>	-- throw EXP							-- TODO: this could be a regular function call
  	do	tok	<- pTok K.Throw
		exp	<- pExp
		return	$ XThrow (spTP tok) exp

  <|>	-- break
  	do	tok	<- pTok K.Break					-- TODO: this could be a regular function call
		return	$ XBreak (spTP tok)


  <|>   -- \ case { ALT .. }
  	-- overlaps with the nexta lambda form
	(Parsec.try $ do
		tok	<- pTok K.BackSlash
		pTok K.Case
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt pSemis)
		return	$ XLambdaCase (spTP tok) alts)

  <|>	-- \ PAT .. -> EXP
	do	tok1	<- pTok K.BackSlash
		pats	<- Parsec.many1 pPat1
		pTok	<- pTok K.RightArrow
		exp	<- pExp
		return	$ XLambdaPats (spTP tok1) pats exp

  <|>	-- [ EXP .. EXP ] / [ EXP, EXP .. EXP ] / [ EXP .. ] etc
	-- overlaps with list comprehensions and list syntax
	Parsec.try pListRange

  <|>	-- [ EXP | QUAL .. ]
	-- overlaps with list syntax
  	(Parsec.try $ do
		tok	<- pTok K.SBra
		exp	<- pExp
		pTok K.Bar
		quals	<- Parsec.sepBy1 pLCQual (pTok K.Comma)
		pTok K.SKet

		return	$ XListComp (spTP tok) exp quals)

  <|>	-- [ EXP, EXP ]
  	(Parsec.try $ do
		tok	<- pTok K.SBra
		exps	<- Parsec.sepBy pExp (pTok K.Comma)
		pTok K.SKet
		return	$ XList (spTP tok) exps)

  <|>	-- ( EXP, EXP .. )
	-- overlaps with ( EXP )
	(Parsec.try $ do
  		tok	<- pTok K.RBra
		exp1	<- pExp
		pTok K.Comma
		exps	<- Parsec.sepBy1 pExp (pTok K.Comma)
		pTok K.RKet
		return	$ XTuple (spTP tok) (exp1 : exps))

  <|>	-- ( EXP )
	-- use XParens to signal to pProj via pExp2 that the expression is wrapped in parens
	--	we need this to distinguish
	--		exp . var		-- field projection
	--		exp . (var)		-- index projection
	--
	do 	exp	<- pRParen pExp
		return 	$ XParens (spX exp) exp
  <?>   "pExp1'"

pListRange :: Parser (Exp SP)
pListRange
 = do
  	tok	<- pTok K.SBra
  	exp1	<- pExp

	(mExp2, mExp3)	<- (Parsec.try $ do	pTok K.DotDot
						pTok K.SKet
						return (Nothing, Nothing))

        	 	<|> (Parsec.try $ do	pTok K.DotDot
						exp	<- pExp
						pTok K.SKet
						return $ (Nothing, Just exp))

        		<|> (Parsec.try $ do	pTok K.Comma
	        				exp2	<- pExp
						pTok K.DotDot
	        				exp3	<- pExp
						pTok K.SKet
						return $ (Just exp2, Just exp3))

			<|> (Parsec.try $ do	pTok K.Comma
	        				exp	<- pExp
						pTok K.DotDot
						pTok K.SKet
						return $ (Just exp, Nothing))

			<?> "List range"

	-- force infinite lists to be lazy
	let lazy = not $ isJust mExp3
	return	$ XListRange (spTP tok) lazy exp1 mExp2 mExp3


-- | Parse an expression in the RHS of a binding or case/match alternative
--	these can have where expressions on the end
pExpRHS :: Parser (Exp SP)
pExpRHS
 = do 	exp	<- pExp
	mWhere	<- 	do 	tok 	<- pTok K.Where
				stmts	<- pCParen $ Parsec.sepEndBy1 pStmt_sigBind pSemis
				return	$ Just (tok, stmts)
		   <|>	return Nothing

	case mWhere of
		Nothing			-> return $ exp
		Just (tok, stmts)	-> return $ XWhere (spTP tok) exp stmts


-- | Parse a projection operator
pProj :: Parser (Exp SP -> Exp SP -> Either String (Exp SP))
pProj
 = 	do	pTok K.Dot
	 	return	(makeProjV JField JIndex)

 <|>	do	pTok K.Hash
 		return	(makeProjV JFieldR JIndexR)
 <?> "pProj"

makeProjV fun funIndex x y
	| XVar sp v	<- y
	= Right $ XProj (spX x) (stripXParens x)
		$ fun 	sp
			v { Var.nameSpace = NameField }

	| XParens sp y'	<- y
	= Right $ XProj (spX x) (stripXParens x)
		$ funIndex
			sp
			(stripXParens y')


	| otherwise
	= Left "pProj: LHS is not a field"

stripXParens (XParens _ x)	= stripXParens x
stripXParens xx			= xx


-- | Parse a list comprehension production / qualifier / guard
pLCQual :: Parser (LCQual SP)
pLCQual
 = 	-- PAT <- EXP
 	-- overlaps with let and guard
 	(Parsec.try $ do
		pat	<- pPat
		pTok K.LeftArrow
		exp	<- pExp
		return	$ LCGen False pat exp)

  <|>	(Parsec.try $ do
		pat	<- pPat
		pTok K.LeftArrowLazy
		exp	<- pExp
		return	$ LCGen True pat exp)

  <|>	do	exp	<- pExp
  		return	$ LCExp exp

  <?>   "pLCQual"

-- Alternatives ------------------------------------------------------------------------------------

-- | Parse a case style alternative
pCaseAlt :: Parser (Alt SP)						-- NOT finished, guards in alts |
pCaseAlt
 = 	-- PAT -> EXP
   do	pat	<- pPat
 	pTok K.RightArrow
	exp	<- pExpRHS
	return	$ APat (spW pat) pat exp


-- | Parse a match style alternative
pMatchAlt :: Parser (Alt SP)
pMatchAlt
 =	-- \| GUARD, ... = EXP
   	do	tok	<- pTok K.Bar
	   	guards	<- (Parsec.sepBy1 pGuard (pTok K.Comma))
		pTok K.Equals
		exp	<- pExpRHS
		return	$ AAlt (spTP tok) guards exp

  <|>	-- \= EXP
  	do	tok	<- pTok K.GuardDefault
		exp	<- pExpRHS
		return	$ ADefault (spTP tok) exp

  <?>   "pMatchAlt"

-- | Parse a guard
pGuard :: Parser (Guard SP)
pGuard
 = 	-- PAT <- EXP
 	-- overlaps with EXP
	(Parsec.try $ do
		pat	<- pPat
	 	pTok K.LeftArrow
		exp	<- pExpRHS
		return	$ GExp (spW pat) pat exp)

	-- EXP
 <|>	do	exp	<- pExpRHS
 		return	$ GBool (spX exp) exp
 <?>   "pGuard"


-- Statements --------------------------------------------------------------------------------------

-- | Parse a signature, statement or binding
pStmt :: Parser (Stmt SP)
pStmt
 = 	-- bindings overlap with expressions
 	(Parsec.try pStmt_sigBind)

  <|>	do	exp	<- pExpRHS
  		return	$ SStmt (spX exp) exp
  <?>   "pStmt"


-- | Parse a bind (only)
pStmt_bind :: Parser (Stmt SP)
pStmt_bind
 = 	-- VAR PAT .. | ALT ..
	-- overlaps with regular binding
 	(Parsec.try $ do
 		var	<- liftM vNameV $ pVar
		pats	<- Parsec.many pPat1
		alts	<- Parsec.many1 pMatchAlt
		return	$ SBindFun (spV var) var pats alts)

	-- VAR PAT = EXPRHS
 <|>	(Parsec.try $ do
 		var	<- liftM vNameV $ pVar
		pats	<- Parsec.many pPat1
		pTok K.Equals
		exp	<- pExpRHS
		return	$ SBindFun (spV var) var pats [ADefault (spV var) exp])

	-- PAT	<- EXPRHS
 <|>	(Parsec.try $ do
 		pat	<- pPat
		pTok K.LeftArrow
		exp	<- pExpRHS
		return	$ SBindMonadic (spW pat) pat exp)

 <|>	-- PAT | ALT ..
 	(Parsec.try $ do
		pat	<- pPat
		alts	<- Parsec.many1 pMatchAlt
		return	$ SBindPat (spW pat) pat (XMatch (spW pat) alts))

 <|>	-- PAT  = EXPRHS
 	do	pat	<- pPat
		pTok K.Equals
		exp	<- pExpRHS
		return	$ SBindPat (spW pat) pat exp

 <?>	"pStmt_bind"


-- | Parse a type sig (only)
pStmt_sig :: Parser (Stmt SP)
pStmt_sig
 = do	vars	<- Parsec.sepBy1 pVar (pTok K.Comma)
	let vars' = map vNameV vars
 	ht	<- pTok K.HasType
	typ	<- pType
	return	$ SSig (spTP ht) vars' typ

-- | Parse a signature or binding
pStmt_sigBind :: Parser (Stmt SP)
pStmt_sigBind
 = 	(Parsec.try pStmt_sig)
  <|> 	pStmt_bind
  <?>   "pStmt_sigBind"

