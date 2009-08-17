{-# OPTIONS -O2 #-}

module Source.Parser.Pattern
	( pPat, pPat2, pPat1
	, pDotLabel)

where


import Source.Exp
import Source.Parser.Base
import qualified Source.Token	as K

import qualified Shared.VarPrim	as Var

import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec


-- Patterns ----------------------------------------------------------------------------------------
pPat :: Parser (Pat SP)
pPat
 = 	(Parsec.try $ do
 		p1	<- pPat2
		pTok K.Colon
		p2	<- pPat
		return	$ WCons (spW p1) p1 p2)

  <|>	pPat2
  <?>   "pPat"

pPat2 :: Parser (Pat SP)
pPat2
 =
   	-- CON { .l1 = PAT1 .. }
  	-- overlaps with CON pats..
  	(Parsec.try $ pPat_conLabel)

	-- CON pats..
  <|>	(do	con		<- pCon
		pats		<- Parsec.many pPat1
		return	$ WCon (spV con) (vNameV con) pats)

  <|>	pPat1
  <?>   "pPat2"

pPat1 :: Parser (Pat SP)
pPat1
 =
	-- '()'
  	do	tok	<- pTok K.Unit
		return	$ WUnit (spTP tok)

  <|>	-- lit
  	do	(lit, sp)	<- pLiteralFmtSP
		return	$ WLit sp lit

  <|>	-- '_'
  	do	tok	<- pTok K.Underscore
		return	$ WWildcard (spTP tok)

  <|>	-- \^VAR
  	do	tok	<- pTok K.Hat
		var		<- pVar
		return	$ WObjVar (spTP tok) (vNameV var)

  <|>	-- [] or [x] or [p1, p2 .. ]
  	do	tok <- pTok K.SBra
		ps	<- Parsec.sepBy pPat (pTok K.Comma)
		pTok K.SKet

		if length ps == 0
			then return	$ WCon (spTP tok) Var.primNil []
			else return	$ WList (spTP tok) ps

  <|>	-- CON { .l1 = PAT1 .. }
  	-- overlaps with CON
  	(Parsec.try $ do
		pPat_conLabel)

  <|>	-- CON
	do	con		<- pCon
		return	$ WCon (spV con) (vNameV con) []

  <|>  	-- VAR @ PAT
 	-- overlaps with VAR
	(Parsec.try $ do
 		var		<- pVar
		pTok K.At
		pat		<- pPat1
		return	$ WAt (spV var) (vNameV var) pat)

  <|> 	-- VAR
  	do	var		<- pVar
		return	$ WVar (spV var) (vNameV var)

  <|>  	-- (PAT, PAT .. )
	-- overlaps with ( PAT )
	(Parsec.try $ do
		pTok K.RBra
		p1		<- pPat
		pTok K.Comma
		ps		<- Parsec.sepBy1 pPat (pTok K.Comma)
		pTok K.RKet

		return	$ WTuple (spW p1) (p1 : ps))

  <|>	-- ( PAT )
  	do	pat		<- pRParen pPat
		return	pat
  <?> "pPat1"

 -- CON { label = PAT ... }
pPat_conLabel :: Parser (Pat SP)
pPat_conLabel
 = do	con		<- pCon
	pTok K.CBra

	let pLabelBind = do
		label	<- pDotLabel
		pTok K.Equals
		pat	<- pPat
		return	(label, pat)

	labelBinds	<- Parsec.sepBy pLabelBind (pTok K.Comma)
	pTok K.CKet

	return	$ WConLabel (spV con) (vNameV con) labelBinds


-- | Parse a label.
pDotLabel :: Parser (Label SP)
pDotLabel
 = 	do	var	<- pVar
 		return	$ LVar (spV var) (vNameF var)

  <|>	do	pTok K.Dot
  		(int, sp)	<- pIntSP
  		return	$ LIndex sp int
  <?>   "pDotLabel"
