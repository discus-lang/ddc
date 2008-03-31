
module Source.Parser.Base
	( module Source.Parser.Util

	, pCParen, pRParen
	, token
	, pTok
	, pVar, pCon, pVarCon
	, pSymbol
	, pConstSP
	, pLitSP
	, pInt )
where

import Source.Exp
import Source.Parser.Util
import qualified Source.Token		as K
import qualified Source.TokenShow	as K
import qualified Text.ParserCombinators.Parsec.Prim	as Parsec

-- Helper Parsers ----------------------------------------------------------------------------------
-- { inside }
pCParen	inside
 = do	pTok K.CBra
	x	<- inside
	pTok K.CKet
	return x

-- ( inside )
pRParen inside
 = do	pTok K.RBra
	x	<- inside
	pTok K.RKet
	return x


-- | Match some token with a function.
token :: (K.TokenP -> Maybe a) -> Parser a
token fun
	= Parsec.token
		(K.showSource . K.token)
		makeParsecSourcePos
		fun


-- Simple Parsers ----------------------------------------------------------------------------------

-- | Match a single token.
pTok :: K.Token -> Parser K.TokenP
pTok tok
 = token	
	(\t -> case t of
		K.TokenP { K.token = tok' }
		 | tok == tok'	-> Just t
		_		-> Nothing)


-- | Parse a variable
pVar :: Parser Var
pVar 
 = token
	(\t -> case t of
		K.TokenP { K.token = K.Var name }	-> Just (makeVar name t)
		_					-> Nothing)


-- | Parse a constructor variable
pCon :: Parser Var
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Con name }	-> Just (makeVar name t)
		_					-> Nothing)


-- | Parse a variable or constructor
pVarCon :: Parser Var
pVarCon
 = token
	(\t -> case t of
		K.TokenP { K.token = K.Var name }	-> Just (makeVar name t)
		K.TokenP { K.token = K.Con name }	-> Just (makeVar name t)
		_					-> Nothing)


-- | Parse a symbolic operator.
--	Several token have special meanings in the type language,
--	but are just regular operators in the term language.
pSymbol :: Parser Var
pSymbol	= token parseSymbol
 where parseSymbol t
	| K.TokenP	{ K.token = tok }	<- t
	= case tok of
		K.Symbol name	-> Just $ toVar t
		K.Colon		-> Just $ toVar t
		K.Star		-> Just $ toVar t
		K.Dash		-> Just $ toVar t
		K.At		-> Just $ toVar t
		K.Hash		-> Just $ toVar t
		K.ABra		-> Just $ toVar t
		K.AKet		-> Just $ toVar t
		K.ForwardSlash	-> Just $ toVar t
		K.Plus		-> Just $ toVar t
		K.Dot		-> Just $ toVar t
		K.Dollar	-> Just $ toVar t
		K.Tilde		-> Just $ toVar t
		K.Percent	-> Just $ toVar t
		_		-> Nothing

	| otherwise
	= Nothing		


-- Literal Constants ------------------------------------------------------------------------------

-- | Parse a boxed or unboxed constant.
pConstSP :: Parser (Const, SP)
pConstSP 
 = 	(Parsec.try $ do
	 	(lit, sp)	<- pLitSP
		pTok K.Hash
		return	(CConstU lit, sp))
  <|>	do
  		(lit, sp)	<- pLitSP
		return	(CConst lit, sp)
		
	
-- | Parse a literal.
pLitSP :: Parser (Literal, SP)
pLitSP = token parseLit 
 where parseLit t
 	| K.TokenP	{ K.token = tok }	<- t
	= case tok of
		K.CInt i	-> Just (LInt i,	spTP t)
		K.CChar	c	-> Just (LChar c,	spTP t)
		K.CFloat f	-> Just (LFloat f,	spTP t)
		K.CString s	-> Just (LString s,	spTP t)
		_		-> Nothing
 	
	| otherwise
	= Nothing

-- | Parse a single integer.
pInt :: Parser (Int, SP)
pInt = token parseInt
 where parseInt t
 	| K.TokenP	{ K.token = K.CInt i }	<- t
	= Just (i, spTP t)
	
	| otherwise
	= Nothing
	

