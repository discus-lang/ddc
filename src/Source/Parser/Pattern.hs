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
 = 	do	p1	<- pPat2
		(	do	pTok K.Colon
				p2	<- pPat
				return	$ WCons (spW p1) p1 p2

		  <|>	do	return	p1)

  <?>   "pPat"

pPat2 :: Parser (Pat SP)
pPat2
 =	-- CON { label = PAT .. }
	-- CON pats..
	do	con	<- pCon
		(	do	lb	<- pCParen $ Parsec.sepBy pLabelBind (pTok K.Comma)
                		return	$ WConLabel (spV con) (vNameV con) lb

		  <|>	do	pats	<- Parsec.many pPat1
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

  <|>	-- CON { label = PAT .. }
  	-- CON
	do	con	<- pCon
		(	do	lb	<- pCParen $ Parsec.sepBy pLabelBind (pTok K.Comma)
                		return	$ WConLabel (spV con) (vNameV con) lb

		  <|>	do	return	$ WCon (spV con) (vNameV con) [])

  <|>  	-- VAR @ PAT
	-- VAR
        do	var		<- pVar
        	(	do	pTok K.At
				pat	<- pPat1
				return	$ WAt (spV var) (vNameV var) pat

		  <|>	do	return	$ WVar (spV var) (vNameV var))

  <|>  	-- (PAT, PAT .. )
	-- ( PAT )
	do	tok	<- pTok K.RBra
		ps	<- Parsec.sepBy1 pPat (pTok K.Comma)
                pTok K.RKet
		if length ps == 1
		 then return	$ head ps
		 else return	$ WTuple (spTP tok) ps

  <?> "pPat1"


pLabelBind :: Parser (Label SP, Pat SP)
pLabelBind
 =	do	label	<- pDotLabel
		pTok K.Equals
		pat	<- pPat
		return	(label, pat)

-- | Parse a label.
pDotLabel :: Parser (Label SP)
pDotLabel
 = 	do	var	<- pVar
		return	$ LVar (spV var) (vNameF var)

  <|>	do	pTok K.Dot
		(int, sp)	<- pIntSP
		return	$ LIndex sp int
  <?>   "pDotLabel"
