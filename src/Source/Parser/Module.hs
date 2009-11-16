{-# OPTIONS -fno-warn-unused-binds -O2 #-}

module Source.Parser.Module
	( parseModule
	, parseString
	, pModule
	, pTopImport)

where
import Source.Parser.Pattern
import Source.Parser.Exp
import Source.Parser.Base
import Source.Parser.Type

import Source.Lexer
import Source.Pretty
import Source.Exp
import qualified Source.Token	as K

import Shared.Pretty
import qualified Shared.Var	as Var

import DDC.Base.NameSpace

import qualified Text.ParserCombinators.Parsec.Prim		as Parsec
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Pos		as Parsec

import Util

-- | Parse a module.
parseModule :: String -> [K.TokenP] -> [Top SP]
parseModule fileName tokens
 = let	eExp	= Parsec.runParser pModule () fileName tokens
   in	case eExp of
   		Right exp	-> exp
		Left err	-> error $ show err

-- run a single parsed on some string,
--	(for testing)
parseString parser str
 = let 	tokens		= scan str
 	eExp		= Parsec.runParser parser () "file" tokens
   in	case eExp of
		Right exp	-> putStr $ (pprStrPlain exp ++ "\n\n")
		Left err	-> error $ show err

-- Module ------------------------------------------------------------------------------------------
-- | Parse a whole module.
pModule :: Parser [Top SP]
pModule =
 do	tops	<- pCParen pOrderedTop
	return	tops

-- | Parse a top level binding, only accepting a specific ordering.
pOrderedTop :: Parser [Top SP]
pOrderedTop
 = do	header	<- Parsec.sepEndBy pTopHeader pSemis
	tops <- Parsec.sepEndBy pTop pSemis
	return	$ header ++ tops

pTopHeader :: Parser (Top SP)
 =	pTopImport
  <|>	pTopExport
  <|>	pTopPragma

pTop :: Parser (Top SP)
pTop
 =
	pTopForeignImport
  <|>	pTopInfix
  <|>	pTopType
  <|>	pTopData
  <|>	pTopEffect
  <|>	pTopRegion
  <|>	pTopClass
  <|>	pTopInstance
  <|>	pTopProject

  <|>	-- SIG/BIND
	do	stmt	<- pStmt_sigBind
		return	$ PStmt stmt

  <?>	"pTop"


-- Pragma ------------------------------------------------------------------------------------------
pTopPragma :: Parser (Top SP)
pTopPragma
 = do	tok	<- pTok K.Pragma
 	exps	<- Parsec.many1 pExp1
	return	$ PPragma (spTP tok) exps


-- Import ------------------------------------------------------------------------------------------
pTopImport :: Parser (Top SP)
pTopImport
 = do	-- import { MODULE ; ... }
   	tok	<- pTok K.Import
	mods	<- pCParen (Parsec.sepEndBy1 pModuleName pSemis)
	return	$ PImportModule (spTP tok) mods

pModuleName :: Parser (Var.Module)
pModuleName
 =
 	-- M1.M2 ..
	do	mod	<- pModuleNameQual
		pTok K.Dot
		con	<- pCon
		return	$ Var.ModuleAbsolute (mod ++ [Var.name con])

 <|>	-- M1
 	do	con	<- pCon
		return	$ Var.ModuleAbsolute [Var.name con]
 <?> "pModuleName"

-- Export -----------------------------------------------------------------------------------------
pTopExport :: Parser (Top SP)
pTopExport
 = do	-- export { EXPORT ; .. }
	tok	<- pTok K.Export
	exps	<- pCParen (Parsec.sepEndBy1 pExport pSemis)
	return	$ PExport (spTP tok) exps

pExport :: Parser (Export SP)
pExport
 = do	-- var
	var	<- pOfSpace NameValue pVarCon
	return	$ EValue (spV var) var

 <|> do	-- type VAR
	tok	<- pTok K.Type
	var	<- pOfSpace NameType pVarCon
	return	$ EType (spTP tok) var

 <|> do	-- region VAR
	tok	<- pTok K.Region
	var	<- pOfSpace NameRegion pVar
	return	$ ERegion (spTP tok) var

 <|> do	-- effect VAR
	tok	<- pTok K.Effect
	var	<- pOfSpace NameEffect pVar
	return	$ EEffect (spTP tok) var

 <|> do	-- class VAR
	tok	<- pTok K.Class
	var	<- pOfSpace NameClass pVar
	return	$ EClass (spTP tok) var

 <?> "pExport"

-- Foreign -----------------------------------------------------------------------------------------
-- Parse a foreign import.
pTopForeignImport :: Parser (Top SP)
pTopForeignImport
 =	do	tok	<- pTok K.Foreign
		pTok K.Import
		fimp	<- pTopForeignImportNext (spTP tok)
		return	$ fimp

pTopForeignImportNext :: SP -> Parser (Top SP)
pTopForeignImportNext startPos
 = 	-- foreign import data STRING CON :: KIND
	do	pTok K.Data
		name	<- pString
		con	<- pOfSpace NameType pCon
		let con2 = con { Var.info = [Var.ISeaName name] }
		fimp	<- pTopForeignImportEnd startPos name con2
		return	$ fimp

 <|>	-- foreign import STRING var :: TYPE
	do	mExName	<- Parsec.optionMaybe pString
		var	<- pOfSpace NameValue pVar
		pTok K.HasType
		sig	<- pType

		mOpType	<- Parsec.optionMaybe
				(do 	pTok K.HasOpType
					pTypeOp)

		return	$ PForeign startPos $ OImport mExName var sig mOpType

 <?> "pTopForeignImport"


pTopForeignImportEnd :: SP -> String -> Var -> Parser (Top SP)
pTopForeignImportEnd startPos name var
 =	do	pTok K.HasType
		kind	<- pKind
		return	$ PForeign startPos (OImportUnboxedData name var kind)

 <|>	do	return	$ PForeign startPos (OImportUnboxedData name var kValue)


-- Infix -------------------------------------------------------------------------------------------
-- Parse an infix definition.
pTopInfix :: Parser (Top SP)
pTopInfix
 = 	-- infixr INT SYM ..
 	do	tok	<- pTok K.InfixR
 		prec	<- pInt
		syms	<- Parsec.sepBy1 (pOfSpace NameValue pSymbol) (pTok K.Comma)
		return	$ PInfix (spTP tok) InfixRight prec syms

	-- infixl INT SYM ..
  <|> 	do	tok	<- pTok K.InfixL
 		prec	<- pInt
		syms	<- Parsec.sepBy1 (pOfSpace NameValue pSymbol) (pTok K.Comma)
		return	$ PInfix (spTP tok) InfixLeft prec syms

  <|>	-- infix INT SYM ..
  	do	tok	<- pTok K.Infix
		prec	<- pInt
		syms	<- Parsec.sepBy1 (pOfSpace NameValue pSymbol) (pTok K.Comma)
		return	$ PInfix (spTP tok) InfixNone prec syms
  <?>   "pTopInfix"

-- Type Kind ---------------------------------------------------------------------------------------
-- | Parse a type kind and type synonym signatures.

pTopType ::  Parser (Top SP)
pTopType
 =	do	tok	<- pTok K.Type
 		exp	<- pTopTypePlus (spTP tok)
		return	$ exp


pTopTypePlus :: SP -> Parser (Top SP)
pTopTypePlus startPos
 =	do	-- type CON :: KIND
		con	<- pOfSpace NameType pCon
		rest	<- pTopTypePlus2 startPos con
		return	$ rest

 <|>	do	-- type VAR ...
		var	<- pOfSpace NameType pVar
		pTok	K.HasType
		t	<- pType
		return	$ PTypeSynonym startPos var t

 <?>	"pTopTypePlus"

pTopTypePlus2 :: SP -> Var -> Parser (Top SP)
pTopTypePlus2 startPos var
 =	do	-- :: TYPE
		pTok	K.HasType
		kind	<- pKind
		return	$ PTypeKind startPos var kind

 <|>	do	-- = TYPE
		pTok	K.Equals
		t	<- pType
		return	$ PTypeSynonym startPos var t

 <?>	"pTopTypePlus2"

-- Effect ------------------------------------------------------------------------------------------
-- | Parse an effect definition.
pTopEffect :: Parser (Top SP)
pTopEffect
 =	-- effect CON :: KIND
 	do	tok	<- pTok K.Effect
		con	<- pOfSpace NameEffect pCon
		pTok	K.HasType
		kind	<- pKind
		return	$ PEffect (spTP tok) con kind


-- Region ------------------------------------------------------------------------------------------
-- | Parse a region definition
pTopRegion :: Parser (Top SP)
pTopRegion
 = 	-- region var
 	do	tok	<- pTok K.Region
		var	<- pOfSpace NameRegion pVar
		return	$ PRegion (spTP tok) var


-- Data --------------------------------------------------------------------------------------------
-- | Parse a data type definition.
pTopData :: Parser (Top SP)
pTopData
 =
 	-- data TYPE = CTOR | ..
  	do	tok	<- pTok	K.Data
		con	<- pOfSpace NameType pCon
		vars	<- liftM (map (vNameDefaultN NameType)) $ Parsec.many pVar

		ctors	<- 	do	pTok K.Equals
					ctors	<- Parsec.sepBy1 pTopDataCtor (pTok K.Bar)
					return	ctors
				<|>	return []

		return	$ PData (spTP tok) con vars ctors

pTopDataCtor :: Parser (Var, [DataField (Exp SP) Type])
pTopDataCtor
 = 	-- CON ...
	do	con	<- pOfSpace NameType pCon
        	rest	<- pTopDataCtorRest con
                return	rest

 <?>    "pTopDataCtor"

pTopDataCtorRest :: Var -> Parser (Var, [DataField (Exp SP) Type])
pTopDataCtorRest con
 =	-- CON { FIELD ; .. }
 	do	fs	<- pCParen $ Parsec.sepEndBy1 pDataField pSemis
		return	(con, fs)

  <|>	-- CON TYPES ..
	do	types	<- Parsec.many pType_body1
		let mkPrimary typ
			= DataField
			{ dPrimary	= True
			, dLabel	= Nothing
			, dType		= typ
			, dInit		= Nothing }
		return	(con, map mkPrimary types)


pDataField :: Parser (DataField (Exp SP) Type)
pDataField
 =  	-- .VAR :: TYPE = EXP
 	do	pTok K.Dot
		var	<- pOfSpace NameField pVar
		pTok K.HasType
		t	<- pType_body
		pTok K.Equals
		exp	<- pExp
		return	$ DataField
			{ dPrimary	= False
			, dLabel	= Just var
			, dType		= t
			, dInit		= Just exp }

  <|>	-- VAR :: TYPE
	(Parsec.try $ do
  		var	<- pOfSpace NameField pVar
	 	pTok K.HasType
		t	<- pType_body

		return	$ DataField
			{ dPrimary	= True
			, dLabel	= Just var
			, dType		= t
			, dInit		= Nothing })

  <|>	do	t	<- pType_body
  		return	$ DataField
			{ dPrimary	= True
			, dLabel	= Nothing
			, dType		= t
			, dInit		= Nothing }

  <?>   "pDataField"

-- Class -------------------------------------------------------------------------------------------
-- | Parse a class definition.
pTopClass :: Parser (Top SP)
pTopClass
 =	do 	tok	<- pTok K.Class
	 	con	<- pOfSpace NameClass pCon
		cls	<- pTopClassMore (spTP tok) con
                return	cls
  <?>   "pTopCLass"

pTopClassMore :: SP -> Var -> Parser (Top SP)
pTopClassMore startPos con
 =	-- class CON :: SUPER
	do	pTok K.HasType
		super	<- pSuper
		return	$ PClass startPos con super

   <|>	-- class CON VAR.. where { SIG ; .. }
	do	vks	<- Parsec.many pVarKind
		pTok K.Where
		sigs	<- pCParen $ Parsec.sepEndBy1 pTopClass_sig pSemis
		return	$ PClassDict startPos con vks [] sigs


-- parse a var with optional kind
pVarKind :: Parser (Var, Kind)
pVarKind
 = 	-- (VAR :: KIND)
	(pRParen $ do
		var	<- liftM (vNameDefaultN NameType) pVar
		pTok K.HasType
		kind	<- pKind
		return	(var, kind))

	-- VAR
 <|>	do	var	<- liftM (vNameDefaultN NameType) pVar
		return	(var, KNil)
 <?>    "pVarKind"



-- VAR, .. :: TYPE
pTopClass_sig :: Parser ([Var], Type)
pTopClass_sig
 = do	vars	<- Parsec.sepBy1 pVar (pTok K.Comma)
	pTok K.HasType
	t	<- pType
	return	$ (vars, t)


-- Instance ----------------------------------------------------------------------------------------
-- | Parse a class instance.
pTopInstance :: Parser (Top SP)
pTopInstance
 = 	-- instance CON TYPE .. where { BIND ; .. }
 	do	tok	<- pTok K.Instance
		con	<- pOfSpace NameClass $ pQualified pCon
		ts	<- Parsec.many pType_body1
		pTok K.Where
		binds	<- pCParen $ Parsec.sepEndBy1 pStmt_bind pSemis
		return	$ PClassInst (spTP tok) con ts [] binds


-- Project -----------------------------------------------------------------------------------------
-- | Parse a projection definition.
pTopProject :: Parser (Top SP)
pTopProject
 =	-- project CON TYPE .....
	do	tok	<- pTok K.Project
		con	<- pOfSpace NameType $ pQualified pCon
		ts	<- Parsec.many pType_body1
                rest	<- pTopProjectRest (spTP tok) con ts
                return	rest
  <?>   "pTopProject"

pTopProjectRest :: SP -> Var -> [Type] -> Parser (Top SP)
pTopProjectRest startPos con ts
 =	-- project CON TYPE .. where { SIG/BIND ; .. }
	do	pTok	K.Where
		binds	<- pCParen $ Parsec.sepEndBy1 pStmt_sigBind pSemis
		return	$ PProjDict startPos (TData KNil con ts) binds

  <|>	-- project CON TYPE .. with { VAR ; .. }
	do	pTok	K.With
		binds	<- pCParen $ Parsec.sepEndBy1 pTopProject_pun pSemis
		return	$ PProjDict startPos (TData KNil con ts) binds

pTopProject_pun :: Parser (Stmt SP)
pTopProject_pun
 = do	-- VAR
	var		<- pOfSpace NameValue pVar
	let varField	= var { Var.nameSpace = NameField }

 	return	$ SBindFun (spV var) varField
			[] 
			[ADefault (spV var) (XVar (spV var) var)]






