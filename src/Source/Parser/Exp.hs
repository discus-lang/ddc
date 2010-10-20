{-# OPTIONS -O2 #-}

module Source.Parser.Exp
	( pExp, pExp1, pExpRHS
	, pStmt, pStmt_bind, pStmt_sig, pStmt_sigBind)
where
import Source.Exp
import Source.Parser.Type
import Source.Parser.Pattern
import Source.Parser.Base
import DDC.Type.SigMode
import DDC.Var
import qualified Source.Token					as K
import qualified Shared.VarPrim					as Var
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec


-- Expressions -------------------------------------------------------------------------------------
-- | Parse an expression
pExp :: Parser (Exp SP)
pExp
 = 	-- as while / when / unless are known to have two arguments
	--	we can relax the need to wrap the second one in parens

 	-- while ( EXP ) EXP  /  while EXP1 EXP
	do	tok	<- pTok K.While
		exp1	<-	pRParen pExp
			<|> 	pExp1

		exp2	<- pExp
		return	$ XWhile (spTP tok) exp1 exp2

 <|>	-- when ( EXP ) EXP  /  when EXP1 EXP
	do	tok	<- pTok K.When
		exp1	<-	pRParen pExp
			<|>	pExp1

		exp2	<- pExp
		return	$ XWhen (spTP tok) exp1 exp2

 <|>	-- unless ( EXP ) EXP  /  when EXP1 EXP
	do	tok	<- pTok K.Unless
		exp1	<-	pRParen pExp
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
 =	-- projections
 	-- EXP . EXP   /   EXP . (EXP)  /  EXP # EXP
	do	exp	<-pExp1'
		(do	x	<- pProject exp
        	        return	$ stripXParens x

		 <|> do return	$ stripXParens exp)

  <?>   "pExp2"

-- | Parse an expression that can be used in an application
pExp1 :: Parser (Exp SP)
pExp1
 = do	exp	<- pExp1'
 	return	$ stripXParens exp

pExp1' :: Parser (Exp SP)
pExp1'
 =
	do	tok	<- pTok K.SBra
		pListContents (spTP tok)

  <|>	-- ()
  	do	tok	<- pTok K.Unit
		return	$ XVar (spTP tok) Var.primUnit

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
  	do	tok	<- pTok K.BackSlashDot
		var	<- pOfSpace NameField pVar
		exps	<- Parsec.many pExp1
		return	$ XLambdaProj (spTP tok) (JField (spTP tok) var) exps

  <|>	-- `VAR`
  	do	pTok K.BackTick
		var	<- pOfSpace NameValue $ pQualified pVar
		pTok K.BackTick
		return	$ XOp (spV var) var

  <|>	-- try EXP catch { ALT .. } (with { STMT; .. })
  	do	tok	<- pTok K.Try
		exp1	<- pExp
		pTok K.Catch
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt pSemis)

		mWith	<-	Parsec.optionMaybe
                		(do	pTok K.With
					stmts	<- pCParen (Parsec.sepEndBy1 pStmt pSemis)
				 	return	$ XDo (spTP tok) stmts)

		return	$ XTry (spTP tok) exp1 alts mWith

  <|>	-- throw EXP							-- TODO: this could be a regular function call
  	do	tok	<- pTok K.Throw
		exp	<- pExp
		return	$ XThrow (spTP tok) exp

  <|>	-- break
  	do	tok	<- pTok K.Break					-- TODO: this could be a regular function call
		return	$ XBreak (spTP tok)

  <|>	do	tok	<- pTok K.BackSlash
		pBackslashExp (spTP tok)

  <|>	-- VARFIELD					-- TODO: change this when we move to parsec
	do	var	<- pOfSpace NameField pVarField
		return	$ XObjField (spV var) var

  <|>	-- SYM
	do	sym	<- pOfSpace NameValue pSymbol
		return	$ XOp (spV sym) sym

  <|>	-- VAR & { TYPE }								-- NOT FINISHED
 	-- overlaps with VAR
	Parsec.try
	  (do	field	<- pOfSpace NameField $ pQualified pVar
		pTok K.And
		t	<- pCParen pType_body
		return	$ XProjT (spV field) t (JField (spV field) field))
			
  <|>	-- VAR/CON
	do	var	<- pOfSpace NameValue $ pQualified pVarCon
		return	$ XVar (spV var) var

  <|>	-- Starts with a K.RBra.
	do	tok	<- pTok K.RBra
  		exp1	<- pExp
		pBracketExp (spTP tok) exp1

  <?>   "pExp1'"


pBracketExp :: SP -> Exp SP -> Parser (Exp SP)
pBracketExp startPos exp1 =
	do	pTok K.Comma
		exps	<- Parsec.sepBy1 pExp (pTok K.Comma)
		pTok K.RKet
		return	$ XTuple startPos (exp1 : exps)

  <|>	do	pTok K.RKet
		return 	$ XParens startPos exp1


pBackslashExp :: SP -> Parser (Exp SP)
pBackslashExp startPos =
	do	pTok K.Case
		alts	<- pCParen (Parsec.sepEndBy1 pCaseAlt pSemis)
		return	$ XLambdaCase startPos alts

  <|>	do	pats	<- Parsec.many1 pPat1
		pTok	<- pTok K.RightArrow
		exp	<- pExp
		return	$ XLambdaPats startPos pats exp


pListContents :: SP -> Parser (Exp SP)
pListContents startPos =
	-- Empty list.
	do	pTok K.SKet
  		return $ XList startPos []

  <|>	-- Either a list, a list comprehension
  	do	first	<- pExp
		pListContentsHaveOne startPos first

  <?>	"pListContents"

pListContentsHaveOne :: SP -> Exp SP -> Parser (Exp SP)
pListContentsHaveOne startPos first =
	-- Single element list.
	do	pTok K.SKet
  		return $ XList startPos [first]

  <|>	-- List comprehension.
	do	pTok K.Bar
		quals	<- Parsec.sepBy1 pLCQual (pTok K.Comma)
		pTok K.SKet
		return	$ XListComp startPos first quals

  <|>	-- Have range expression without step.
	do	pTok K.DotDot
		pListRangeOne startPos first

  <|>	-- Have one element and Comma
	do	pTok K.Comma
        	second	<- pExp
		pListContentsHaveTwo startPos first second

  <?>	"pListContentsHaveOne"

pListRangeOne :: SP -> Exp SP -> Parser (Exp SP)
pListRangeOne startPos first =
	do	pTok K.SKet
		return $ XListRange startPos True first Nothing Nothing

  <|>	do	exp <- pExp
		pTok K.SKet
		return $ XListRange startPos False first Nothing (Just exp)

  <?>	"pListRangeOne"


pListContentsHaveTwo :: SP -> Exp SP -> Exp SP -> Parser (Exp SP)
pListContentsHaveTwo startPos first second =
	do	pTok K.DotDot
		pListRangeTwo startPos first second

  <|>	do	pTok K.SKet
		return $ XList startPos [first, second]

  <|>	do	pTok K.Comma
  		rest <- Parsec.sepBy1 pExp (pTok K.Comma)
		pTok K.SKet
		return $ XList startPos (first : second : rest)

  <?>	"pListContentsHaveTwo"

pListRangeTwo :: SP -> Exp SP -> Exp SP -> Parser (Exp SP)
pListRangeTwo startPos first second =
	do	pTok K.SKet
		return $ XListRange startPos True first (Just second) Nothing

  <|>	do	exp <- pExp
		pTok K.SKet
		return $ XListRange startPos False first (Just second) (Just exp)

  <?>	"pListRangeTwo"


-- | Parse an expression in the RHS of a binding or case/match alternative
--	these can have where expressions on the end
pExpRHS :: Parser (Exp SP)
pExpRHS
 = do 	exp	<- pExp
	mWhere	<- Parsec.optionMaybe
        		(do 	tok 	<- pTok K.Where
				stmts	<- pCParen $ Parsec.sepEndBy1 pStmt_sigBind pSemis
				return	(tok, stmts))

	case mWhere of
		Nothing			-> return exp
		Just (tok, stmts)	-> return $ XWhere (spTP tok) exp stmts


-- Recursively apply the projection combinator
pProject :: Exp SP -> Parser (Exp SP)
pProject x
 =	do	pProj2 x >>= pProject
 <|>	return x


-- | Parse a projection operator
pProj2 :: Exp SP -> Parser (Exp SP)
pProj2 x
 = 	do	pTok K.Dot
	 	makeProjV JField JIndex x

 <|>	do	pTok K.Hash
 		makeProjV JFieldR JIndexR x


makeProjV fun funIndex x
 = do	y	<- pExp1'
	case y of
	  XVar sp v
	    -> return	$ XProj (spX x) (stripXParens x)
			$ fun sp v { varNameSpace = NameField }

	  XParens sp y'
            -> return	$ XProj (spX x) (stripXParens x)
			$ funIndex sp (stripXParens y')

	  _ -> Parsec.unexpected "Projection: LHS is not a field"


stripXParens (XParens _ x)	= stripXParens x
stripXParens xx			= xx


-- | Parse a list comprehension production / qualifier / guard
pLCQual :: Parser (LCQual SP)
pLCQual
 =	-- LET VAR ...
	do	pTok K.Let
                ss	<- pCParen (Parsec.sepEndBy1 pLCQualLet pSemis)
                return	$ LCLet ss

 <|> 	-- PAT <- EXP
 	-- overlaps with let and guard
	Parsec.try
	  (do	pat	<- pPat
		lazy	<- pLeftArrowIsLazy
		exp	<- pExp
		return	$ LCGen lazy pat exp)

  <|>	do	exp	<- pExp
		return	$ LCExp exp

  <?>   "pLCQual"


pLCQualLet :: Parser (Stmt SP)
pLCQualLet
 =	do	var	<- pOfSpace NameValue pVar
		pats	<- Parsec.many pPat1
		stmt	<- pStmt_bindVarPat var pats
		return	stmt

 <|>	do	pat	<- pPat
		stmt	<- pStmt_bindPat2 pat
		return	stmt


pLeftArrowIsLazy :: Parser Bool
pLeftArrowIsLazy =
	do	pTok K.LeftArrowLazy
        	return True

  <|>	do	pTok K.LeftArrow
  		return False


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

  <?>   "a pattern match alternative"

-- | Parse a guard
pGuard :: Parser (Guard SP)
pGuard
 = 	-- PAT <- EXP
 	-- overlaps with EXP
	Parsec.try
	  (do	pat	<- pPat
	 	pTok K.LeftArrow
		exp	<- pExpRHS
		return	$ GExp (spW pat) pat exp)

	-- EXP
 <|>	do	exp	<- pExpRHS
 		return	$ GBool (spX exp) exp
 <?>   "a guard"


-- Statements --------------------------------------------------------------------------------------

-- | Parse a signature, statement or binding
pStmt :: Parser (Stmt SP)
pStmt
 = 	-- bindings overlap with expressions
 	Parsec.try pStmt_sigBind

  <|>	do	exp	<- pExpRHS
  		return	$ SStmt (spX exp) exp
  <?>   "a statement"


-- | Parse a bind (only)
pStmt_bind :: Parser (Stmt SP)
pStmt_bind
 =	-- LET VAR ....
 	do	pTok K.Let
		pTok K.CBra
		var	<- pOfSpace NameValue pVar
		pats	<- Parsec.many pPat1
		stmt	<- pStmt_bindVarPat var pats
		pSemis
		pTok K.CKet
		return	stmt

 <|>	-- VAR ....
	Parsec.try
          (do	var	<- pOfSpace NameValue pVar
		pats	<- Parsec.many pPat1
		pStmt_bindVarPat var pats)

 <|>	-- PAT  ...
	  (do	pat	<- pPat
		pStmt_bindPat2 pat)

 <?>	"pStmt_bind"


pStmt_bindVarPat :: Var -> [Pat SP] -> Parser (Stmt SP)
pStmt_bindVarPat var pats
 =	-- VAR PAT = EXPRHS
	do	pTok K.Equals
		exp	<- pExpRHS
		return	$ SBindFun (spV var) var pats [ADefault (spV var) exp]

 <|>	-- VAR PAT .. | ALT ..
 	do	alts	<- Parsec.many1 pMatchAlt
		return	$ SBindFun (spV var) var pats alts

 <?>	"variable binding"


pStmt_bindPat2 :: Pat SP -> Parser (Stmt SP)
pStmt_bindPat2 pat
 =	-- PAT  = EXPRHS
 	do	pTok K.Equals
		exp	<- pExpRHS
		return	$ SBindPat (spW pat) pat exp

 <|>	-- PAT	<- EXPRHS
	do	pTok K.LeftArrow
		exp	<- pExpRHS
		return	$ SBindMonadic (spW pat) pat exp

 <|>	-- PAT | ALT ..
 	do	alts	<- Parsec.many1 pMatchAlt
		return	$ SBindPat (spW pat) pat (XMatch (spW pat) alts)

 <?> 	"pattern binding"


-- | Parse a type sig (only)
pStmt_sig :: Parser (Stmt SP)
pStmt_sig
 = Parsec.sepBy1 (pOfSpace NameValue pVar) (pTok K.Comma) 
 >>= \vars 
 -> 	do 	ht	<- pTok K.HasTypeMatch
	   	typ	<- pType <?> "a type for " ++ quotVars vars
	   	return	$ SSig (spTP ht) SigModeMatch vars typ

 <|> 	do 	ht	<- pTok K.HasTypeExact
	   	typ	<- pType <?> "a type for " ++ quotVars vars
	   	return	$ SSig (spTP ht) SigModeExact vars typ

 <|>	do 	ht	<- pTok K.HasTypeLess
	   	typ	<- pType <?> "a type for " ++ quotVars vars
	   	return	$ SSig (spTP ht) SigModeLess vars typ
	
 <|>	do 	ht	<- pTok K.HasTypeMore
	   	typ	<- pType <?> "a type for " ++ quotVars vars
	   	return	$ SSig (spTP ht) SigModeMore vars typ

 <?> "a type signature"

-- | Parse a signature or binding
pStmt_sigBind :: Parser (Stmt SP)
pStmt_sigBind
 = 	Parsec.try pStmt_sig
  <|> 	pStmt_bind
  <?>   "pStmt_sigBind"

