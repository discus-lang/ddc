{-# OPTIONS -O2 #-}

module Source.Parser.Base
	( module Source.Parser.Util

	-- Helper parsers
	, pCParen, pRParen, pSParen
	, token
	, pTok, pQualified
	, pVar, pOfSpace, pVarPlain, pVarPlainNamed, pVarPlainOfSpace, pVarField
	, pVarSpaceHasType
	, pCon, pConNamed, pConOfSpace, pConOfSpaceNamed
	, pVarCon
	, pSymbol
	, pSemis
	, pModuleNameQual

	-- Literal Constants
	, pLiteralFmtSP
	, pInt, pIntSP, pZero
	, pString, pStringSP

	-- Debugging and tracing combinators.
	, pAnything, pTrace, pShowNext )
where
import Source.Parser.Util
import Control.Monad
import Debug.Trace
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Var
import Data.List						(intercalate)
import qualified Source.Token					as K
import qualified Source.TokenShow				as K
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec

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
 = 	Parsec.try
	  (do
 		mods	<- pModuleNameQual
 		pTok K.Dot
		v	<- parser
		return	$ v { varModuleId = ModuleId mods })
 <|>	parser
 <?>    "pQualified"


-- Variables ---------------------------------------------------------------------------------------

-- | Parse a var from a certain namespace.
--	If the var had a namespace qualifier in the source file then the varNameSpace
--	field will already be set. If the requested namespace is different, then the parse fails.
--
--	If the var's namespace is NameNothing, then we it didn't have a qualifier, and we
--	safely rename it to NameValue, NameType, NameField or NameClass
--
pOfSpace :: NameSpace -> Parser Var -> Parser Var
pOfSpace spaceWant parser
 = do	var <- parser
	case (varNameSpace var, spaceWant) of
	 (NameNothing, NameValue)	-> return var { varNameSpace = NameValue }
	 (NameNothing, NameType)	-> return var { varNameSpace = NameType }	
	 (NameNothing, NameClass)	-> return var { varNameSpace = NameClass }
	 (NameNothing, NameField)	-> return var { varNameSpace = NameField}
	 (space1,      space2)
		| space1 == space2	-> return var
		| otherwise		-> Parsec.unexpected (varName var)


-- | Parse a plain or (symbol) variable
pVar :: Parser Var
pVar =
        pVarPlain
  <|>	Parsec.try (pRParen pSymbol)
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
		    in	if elem (varNameSpace var) spaces
		   		then Just var
				else Nothing
		_ -> Nothing)


-- | Parse a var from a certain namespace followed by a HasType token.
pVarSpaceHasType :: NameSpace -> Parser Var
pVarSpaceHasType spaceWant
 = Parsec.try
 $ do	var	<- pOfSpace spaceWant pVar
	pTok	K.HasTypeMatch
	return	var


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

-- | Parse a constructor
pConNamed :: String -> Parser Var
pConNamed str 
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Con name }	
			| str == name	-> Just $ toVar t
			| otherwise	-> Nothing
		_			-> Nothing)

-- | Parse a constructor, but only from certain name spaces
pConOfSpace :: [NameSpace] -> Parser Var
pConOfSpace spaces
 = token
 	(\t -> case t of
		K.TokenP { K.token = K.Con name }
		 -> let	var	= toVar t
		    in	if elem (varNameSpace var) spaces
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
		    in	if   elem (varNameSpace var) spaces
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


-- | Parse a literal zero.
pZero	:: Parser ()
pZero	= token parseZero
 where parseZero t
	| K.TokenP
	{ K.token = K.Literal (LiteralFmt (LInt 0) Boxed) }	<- t
	= Just ()
	
	| otherwise
	= Nothing

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



--------------------------------------------------------------------------------
-- Debugging and tracing combinators.

-- Grab the next token and return it as a string.
pAnything :: Parser String
pAnything
 = token (\t -> Just (show t))


-- Print the supplied string and the next token.
pTrace :: String -> Parser ()
pTrace s
 = 	do	Parsec.try
 		  $ do	let spaces = take (20 - length s) (repeat ' ')
			x <- pAnything
			trace (s ++ spaces ++ ": " ++ x) $ fail ""

 <|>	return ()


-- Print out the next 'count' tokens. Do not consume input.
pShowNext :: Int -> Parser ()
pShowNext count
 =	do	Parsec.try
		 $ do	x <- grabCount count []
			trace ("pShowNext :\n    "
				++ (intercalate "\n    " x)
				++ "\n\n")
				$ fail ""

 <|>	return ()


grabCount :: Int -> [String] -> Parser [String]
grabCount count accum
 = if count <= 0
	then return $ reverse accum
	else
	  do	x <- pAnything
		grabCount (count - 1) (x : accum)

