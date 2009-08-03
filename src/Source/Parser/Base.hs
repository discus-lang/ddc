{-# OPTIONS -O2 #-}


module Source.Parser.Base
	( module Source.Parser.Util

	-- Helper parsers
	, pCParen, pRParen, pSParen
	, token
	, pTok, pQualified
	, pVar, pVarPlain, pVarPlainNamed, pVarPlainOfSpace, pVarField
	, pCon, pConOfSpace, pConOfSpaceNamed
	, pVarCon
	, pSymbol
	, pSemis
	, pModuleNameQual

	-- Literal Constants
	, pLiteralFmtSP
	, pInt, pIntSP
	, pString, pStringSP )
where

import Source.Exp
import Source.Parser.Util
import qualified Source.Token		as K
import qualified Source.TokenShow	as K

import qualified Shared.Var		as Var
import Shared.VarSpace			(NameSpace(..))
import Shared.Base

import qualified Text.ParserCombinators.Parsec.Prim		as Parsec
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import Control.Monad

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

-- [ inside ]
pSParen inside
 = do	pTok K.SBra
	x	<- inside
	pTok K.SKet
	return x


-- | Match some token with a function.
token :: (K.TokenP -> Maybe a) -> Parser a
token fun
	= Parsec.token
		(K.showSource . K.token)
		makeParsecSourcePos
		fun

-- | Match a single token.
pTok :: K.Token -> Parser K.TokenP
pTok tok
 = token
	(\t -> case t of
		K.TokenP { K.token = tok' }
		 | tok == tok'	-> Just t
		_		-> Nothing)

-- | Parse some semi colons
pSemis :: Parser ()
pSemis
 = do	Parsec.many1 (pTok K.SemiColon)
 	return ()


-- | Parse a module name qualifier
pModuleNameQual :: Parser [String]
pModuleNameQual = token parseMod
 where parseMod t
 	| K.TokenP { K.token = K.ModuleName ss } <- t
	= return ss

	| otherwise
	= Nothing

pQualified :: Parser Var -> Parser Var
pQualified parser
 = 	(Parsec.try $ do
 		mods	<- pModuleNameQual
 		pTok K.Dot
		v	<- parser
		return	$ v { Var.nameModule = Var.ModuleAbsolute mods })
 <|>	parser
 <?>    "pQualified"


-- Variables ---------------------------------------------------------------------------------------

-- | Parse a plain or (symbol) variable
pVar :: Parser Var
pVar =
        pVarPlain
  <|>	(Parsec.try $ pRParen pSymbol)
  <?>   "pVar"


-- | Parse a plain variable
pVarPlain :: Parser Var
pVarPlain
 = token
	(\t -> case t of
		K.TokenP { K.token = K.Var name }	-> Just $ toVar t
		_					-> Nothing)

-- | Parse a plain var with a specific name
pVarPlainNamed :: String -> Parser Var
pVarPlainNamed str
 = token
	(\t -> case t of
		K.TokenP { K.token = K.Var name }
			| name == str		-> Just $ toVar t
			| otherwise		-> Nothing
		_				-> Nothing)



-- | Parse a plain variable, but only from certain name spaces
pVarPlainOfSpace :: [NameSpace] -> Parser Var
pVarPlainOfSpace spaces
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Var name }
		 -> let	var	= toVar t
		    in	if elem (Var.nameSpace var) spaces
		   		then Just var
				else Nothing
		_ -> Nothing)


-- | Parse a object field name
--	TODO: can ditch this when we move to the new parser
pVarField :: Parser Var
pVarField
 = token
	(\t -> case t of
		K.TokenP { K.token = K.VarField name }	-> Just $ toVar t
		_					-> Nothing)


-- Constructors ------------------------------------------------------------------------------------
-- | Parse a constructor
pCon :: Parser Var
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Con name }	-> Just $ toVar t
		_					-> Nothing)

-- | Parse a constructor, but only from certain name spaces
pConOfSpace :: [NameSpace] -> Parser Var
pConOfSpace spaces
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Con name }
		 -> let	var	= toVar t
		    in	if elem (Var.nameSpace var) spaces
		   		then Just var
				else Nothing
		_ -> Nothing)

-- | Parse a certain constructor in a given namespace, with a specific name
pConOfSpaceNamed :: [NameSpace] -> String -> Parser Var
pConOfSpaceNamed spaces str
 = token
	(\t -> case t of
		K.TokenP { K.token = K.Con name }
		 -> let var	= toVar t
		    in	if   elem (Var.nameSpace var) spaces
		          && name == str
			  then Just var
			  else Nothing

		_	-> Nothing)


-- | Parse a variable or constructor
pVarCon :: Parser Var
pVarCon	= pVar <|> pCon <?> "pVarCon"



-- Symbols -----------------------------------------------------------------------------------------
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
		K.Dollar	-> Just $ toVar t
		K.Tilde		-> Just $ toVar t
		K.Percent	-> Just $ toVar t
		_		-> Nothing

	| otherwise
	= Nothing


-- Literal Constants ------------------------------------------------------------------------------

-- | Parse a boxed or unboxed constant.
{-
pConstSP :: Parser (Const, SP)
pConstSP
 = 	(Parsec.try $ do
 		(lit, sp)	<- pUnboxedBoolSP
		return	(CConstU lit, sp))

  <|>   (Parsec.try $ do
	 	(lit, sp)	<- pLitSP
		pTok K.Hash
		return	(CConstU lit, sp))
  <|>	do
  		(lit, sp)	<- pLitSP
		return	(CConst lit, sp)

-- | Parse an unboxed boolean token
--	ie  true# or false#
pUnboxedBoolSP :: Parser (LiteralFmt, SP)
pUnboxedBoolSP = token parseBool
 where parseBool t
 	| K.TokenP	{ K.token = tok }	<- t
	= case tok of
		K.Literal litFmt@(LiteralFmt (LBool b) Unboxed)
				-> Just (litFmt, spTP t)
		_		-> Nothing

	| otherwise
	= Nothing
-}

-- | Parse a literal.
pLiteralFmtSP :: Parser (LiteralFmt, SP)
pLiteralFmtSP = token parseLit
 where parseLit t
 	| K.TokenP	{ K.token = tok }	<- t
	= case tok of
		K.Literal litFmt -> Just (litFmt, spTP t)
		_		 -> Nothing

	| otherwise
	= Nothing


-- | Parse a plain single integer.
pIntSP :: Parser (Int, SP)
pIntSP = token parseInt
 where parseInt t
 	| K.TokenP
	{ K.token = K.Literal (LiteralFmt (LInt i) Boxed) }	<- t
	= Just (fromIntegral i, spTP t)

	| otherwise
	= Nothing

pInt	= liftM fst pIntSP


-- | Parse a single string
pStringSP :: Parser (String, SP)
pStringSP = token parseString
 where parseString t
 	| K.TokenP
	{ K.token = K.Literal (LiteralFmt (LString s) Boxed) } <- t
	= Just (s, spTP t)

	| otherwise
	= Nothing

pString	= liftM fst pStringSP

