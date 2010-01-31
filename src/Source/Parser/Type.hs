{-# OPTIONS -O2 #-}

module Source.Parser.Type
	( pSuper
	, pKind
	, pType, pType_body, pType_body1, pTypeOp)

where

import Type.Util
import Type.Exp

import Source.Parser.Base
import qualified Source.Token	as K

import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var

import DDC.Base.NameSpace

import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec

import Control.Monad
import Data.Maybe

-- Super -------------------------------------------------------------------------------------------
pSuper :: Parser Super
pSuper
 = 	do	pTok K.Plus
		return SProp

 <|>	-- KIND -> SUPER
	do	k1	<- pKind1
                pTok K.RightArrow
		s2	<- pSuper
		return $ SFun k1 s2
		
 <?>    "pSuper"	


-- Kind --------------------------------------------------------------------------------------------
pKind :: Parser Kind
pKind
 = 	-- KIND -> KIND
	do	k1	<- pKind1

	        Parsec.option k1
                 $ do	pTok K.RightArrow
			k2	<- pKind
			return $ KFun k1 k2

 <?>    "pKind"

pKind1 :: Parser Kind
pKind1
 = 	do	pTok K.Star
 		return	kValue

 <|>	do	pTok K.Percent
 		return	kRegion

 <|>	do	pTok K.Bang
 		return	kEffect

 <|>	do	pTok K.Dollar
 		return	kClosure

-- <|>	do	pTok K.Plus
--		return	KWitness

 <|>	-- ( KIND )
 	do	pRParen pKind

 <?>    "pKind1"


-- Type --------------------------------------------------------------------------------------------

-- Parse a type.
pType :: Parser Type
pType
 =  	-- forall VAR .. . TYPE
 	do	tok	<- pTok K.Forall
		vks	<- Parsec.many1 pVar_withKind
		pTok K.Dot
		body	<- pType_bodyFetters
		return	$ makeTForall_back vks body

 <|>	pType_bodyFetters
 <?>    "pType"

-- Parse a quantified variable, with optional kind
pVar_withKind :: Parser (Var, Kind)
pVar_withKind
 = 	pRParen pVar_withKind1
 <|>	pVar_withKind1
 <?>    "pVar_withKind"

pVar_withKind1 :: Parser (Var, Kind)
pVar_withKind1
 =	do	var	<- liftM (vNameDefaultN NameType) pVarPlain
		(	do	-- VAR :: KIND
				pTok K.HasType
				kind	<- pKind
				return	(var, kind)

		 <|>	-- VAR
                  	do	return (var,
                        		kindOfVarSpace (Var.nameSpace var)))

 <?>    "pVar_withKind1"

-- Parse a body type with an optional context and constraint list
pType_bodyFetters :: Parser Type
pType_bodyFetters
 = do	mContext	<- Parsec.optionMaybe
				(Parsec.try pType_someContext)

  	body		<- pType_body

 	mFetters	<- Parsec.optionMaybe
                	(do	pTok K.HasConstraint
				fetters	<- Parsec.sepBy1 pFetter (pTok K.Comma)
				return $ fetters)

	case concat $ maybeToList mContext ++ maybeToList mFetters of
		[]	-> return body
		fs	-> return $ TFetters body fs

pType_someContext :: Parser [Fetter]
pType_someContext
 = do	fs <- pType_hsContext
	pTok K.RightArrowEquals
        return fs

 <|>	pType_context []


-- Parse some class constraints written as a Disciple context
--	C1 => C2 => C3 ...
pType_context :: [Fetter] -> Parser [Fetter]
pType_context accum
 =	-- CONTEXT => CONTEXT ..
	do	fs	<- pType_classConstraint
 		pTok K.RightArrowEquals
		pType_context (fs : accum)

 <|>	return accum

-- Parser some class constraints written as a Haskell context
--	(ClassConstraint ,ClassConstraint*)
pType_hsContext :: Parser [Fetter]
pType_hsContext
 = 	-- (CONTEXT, ..)
 	do	cs	<- pRParen $ Parsec.sepBy1 pType_classConstraint (pTok K.Comma)
 		return	cs

-- Parse a single type class constraint
--	Con Type*
pType_classConstraint :: Parser Fetter
pType_classConstraint
 =	do	con	<- pOfSpace NameClass pCon
	 	ts	<- Parsec.many1 pType_body1
		return	$ FConstraint con ts



-- Parse a body type (without a forall or fetters)
pType_body :: Parser Type
pType_body
 = 	-- TYPE -> TYPE
	-- TYPE -(EFF/CLO)> TYPE
	-- TYPE -(EFF)> TYPE
	-- TYPE -(CLO)> TYPE
	do	t1	<- pType_body3

		mRest	<- Parsec.optionMaybe
			(	-- TYPE -> TYPE
				do	pTok K.RightArrow
					t2	<- pType_body
					return	$ (tPure, tEmpty, t2)

				-- TYPE -(EFF/CLO)> TYPE
			  <|>	do	pTok K.Dash
					pTok K.RBra
					typ	<- pTypeDashRBra t1
					return $ typ)

		case mRest of
			Just (eff, clo, t2)	-> return $ makeTFun t1 t2 eff clo
			_			-> return $ t1


pTypeDashRBra :: Type -> Parser (Type, Type, Type)
pTypeDashRBra t1
 =	-- EFF/CLO)> TYPE
	-- EFF)> TYPE
	do	eff	<- pEffect
		clo	<- Parsec.option tEmpty pClosure
		pTok K.RKet
		pTok K.AKet
		t2	<- pType_body
		return	$ (eff, clo, t2)

  <|>	-- CLO)> TYPE
	do	clo	<- pClosure
		pTok K.RKet
		pTok K.AKet
		t2	<- pType_body
		return $ (tPure, clo, t2)


pType_body3 :: Parser Type
pType_body3
 = pType_body2 >>= \t ->

	-- TYPE {read}
 	(do	elab	<- pCParen
		    $	(do	pVarPlainNamed "read"
				return ElabRead)

		    <|>	(do	pVarPlainNamed "write"
				return ElabWrite)

		    <|>	(do	pVarPlainNamed "modify"
				return ElabModify)

		return	$ TElaborate elab t)

	-- TYPE
   <|>		return	t
   <?>      "pType_body3"

-- | Parse a type that can be used as an argument to a function constructor
pType_body2 :: Parser Type
pType_body2
 =	-- CON TYPE..
	do	con	<- pOfSpace NameType $ pQualified pCon
 		args	<- Parsec.many pType_body1
		return	$ makeTData con KNil args

 <|>	do	t1	<- pType_body1
		Parsec.option t1
			(do	ts	<- Parsec.many1 pType_body1
				return	$ makeTApp (t1:ts))

 <?>    "pType_body2"

-- | Parse a type that can be used as an argument to a type constructor
pType_body1 :: Parser Type
pType_body1
 = 	-- ()
 	do	pTok K.Unit
		return	$ makeTData (Var.primTUnit) kValue []

 <|>	-- [ TYPE , .. ]
 	do	ts	<- pSParen $ Parsec.sepBy1 pType_body (pTok K.Comma)
		return	$ makeTData 
				Var.primTList
				(KFun (KFun kRegion kValue) kValue)
				ts

 <|>	-- *Bot
	(Parsec.try $ do
		con	<- pConNamed "*Bot"
		return	$ TBot kValue)

 <|>	-- %Bot
	(Parsec.try $ do
		con	<- pConNamed "%Bot"
		return	$ TBot kRegion)

 <|>	-- !Bot
	(Parsec.try $ do
		con	<- pConNamed "!Bot"
		return	$ TBot kEffect)

 <|>	-- \$Bot
	(Parsec.try $ do
		con	<- pConNamed "$Bot"
		return	$ TBot kClosure)

	-- VAR
 	-- If a variable had no namespace qualifier out the front the lexer will leave
	--	it in NameNothing. In this case we know its actually a type variable, so can
	--	set it in NameType.
 <|>	do	var	<- liftM (vNameDefaultN NameType) $ pVarPlain
		return	$ TVar 	(kindOfSpace $ Var.nameSpace var) var
		
 <|>	-- CON
 	do	con	<- pOfSpace NameType $ pQualified pCon
		return	$ makeTData con KNil []

 <|>	pRParen pTypeBodyInRParen

 <?>    "pType_body1"


pTypeBodyInRParen :: Parser Type
pTypeBodyInRParen
 =	-- (VAR :: KIND)
	(Parsec.try $ do
		var	<- pOfSpace NameType $ pVarPlain
		pTok K.HasType
		kind	<- pKind

		return	$ TVar kind var)

 <|>	-- (CON :: KIND)
 	(Parsec.try $ do
		con	<- pOfSpace NameType $ pQualified pCon
		pTok K.HasType
		kind	<- pKind

		return	$ makeTData con kind [])

 <|>	-- ( TYPE, TYPE .. )
	-- ( TYPE )
	do	ts	<- Parsec.sepBy1 pType_body (pTok K.Comma)
                if length ts == 1
                 then return	$ head ts
                 else return	$ makeTData 
					(Var.primTTuple (length ts))
					(KFun (KFun kRegion kValue) kValue)
					ts


-- Effect ------------------------------------------------------------------------------------------
-- | Parse an effect
pEffect :: Parser Type
pEffect
 = 	-- VAR
 	do	var	<- pVarPlainOfSpace [NameEffect]
		return $ TVar kEffect var

 <|>	-- !{ EFF; .. }
 	do	pTok	K.Bang
		effs	<- pCParen $ Parsec.sepEndBy1 pEffect pSemis
		return	$ TSum kEffect effs

 <|>	-- !SYNC
	do	var	<- pConOfSpaceNamed [NameEffect] "SYNC"
 		return	$ TTop kEffect

 <|>	-- !CON TYPE..
	do	con	<- pOfSpace NameEffect $ pQualified pCon
 		ts	<- Parsec.many pType_body1
		return	$ TEffect con ts
 <?>    "pEfect"


-- Closure -----------------------------------------------------------------------------------------
-- | Parse a closure
pClosure :: Parser Type
pClosure
  	-- VAR :  CLO
  	-- VAR :  TYPE
	-- VAR $> VAR
 =	(Parsec.try 
	  $ do	var	<- pQualified pVar
	 
	 	do	pTok K.Colon
			let varN	= vNameDefaultN NameValue var
			-- VAR :  CLO
			do	clo	<- pClosure
				return $ TFree varN clo

		  	-- VAR :  TYPE
		   	 <|> do	typ	<- pType
 				return	$ TFree varN typ

		-- VAR $> CLO
	  	 <|> do	pTok K.HoldsMono
			var2	<- pVarPlain
			let varN	= vNameDefaultN NameType var
	 		let var2N	= vNameDefaultN NameType var2
			return	$ TDanger
					(TVar kRegion varN)
					(TVar (kindOfSpace $ Var.nameSpace var2N) var2N))


 <|>	-- \${ CLO ; .. }
 	do	pTok	K.Dollar
		clos	<- pCParen $ Parsec.sepEndBy1 pClosure pSemis
		return	$ TSum kClosure clos

 <|>	-- VAR
	do	var	<- pVarPlainOfSpace [NameClosure]
		return	$ TVar kClosure var
 <?>    "pClosure"

-- Fetter ------------------------------------------------------------------------------------------
-- | Parse a fetter
pFetter :: Parser Fetter
pFetter
 =  	-- CON TYPE..
	do	con	<- pOfSpace NameClass $ pQualified pCon
		ts	<- Parsec.many pType_body1
		return	$ FConstraint con ts


 <|>	-- VAR =  EFFECT/CLOSURE
	-- VAR :> EFFECT/CLOSURE
	(pVarPlainOfSpace [NameEffect, NameClosure] >>= \var ->
		-- VAR = EFFECT/CLOSURE
 		(do	pTok K.Equals
			effClo	<- pEffect <|> pClosure
			return	$ FWhere (TVar (kindOfSpace $ Var.nameSpace var) var)
					 effClo)

		-- VAR :> EFFECT/CLOSURE
	  <|>	(do	pTok K.IsSuptypeOf
			effClo	<- pEffect <|> pClosure
			return	$ FMore (TVar (kindOfSpace $ Var.nameSpace var) var)
					effClo))
 <?>    "pFetter"

-- TypeOp ------------------------------------------------------------------------------------------

-- Parse an operational type
pTypeOp :: Parser Type
pTypeOp
 =	do	t1	<- pTypeOp1
		Parsec.option t1
                	(do	pTok K.RightArrow
				t2	<- pTypeOp
				return	$ makeTFun t1 t2 tPure tEmpty)

 <?>    "pTypeOp"

pTypeOp1 :: Parser Type
pTypeOp1
 =	-- CON
	do	con	<- pOfSpace NameType $ pQualified pCon
		ts	<- Parsec.many pTypeOp1
		return	$ makeTData con KNil ts

