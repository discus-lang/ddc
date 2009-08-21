{-# OPTIONS -O2 #-}

module Source.Parser.Type
	(pKind, pType, pType_body, pType_body1, pTypeOp)

where

import Type.Util
import Type.Exp

import Source.Parser.Base
import qualified Source.Token	as K

import Shared.VarSpace		(NameSpace(..))
import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var

import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec

import Control.Monad
import Data.Maybe

-- Kind --------------------------------------------------------------------------------------------
pKind :: Parser Kind
pKind
 = 	-- KIND -> KIND
	do	k1	<- pKind1

	        Parsec.option k1
                	(do	pTok K.RightArrow
				k2	<- pKind
				return $ KFun k1 k2)

 <?>    "pKind"

pKind1 :: Parser Kind
pKind1
 = 	do	pTok K.Star
 		return	KValue

 <|>	do	pTok K.Percent
 		return	KRegion

 <|>	do	pTok K.Bang
 		return	KEffect

 <|>	do	pTok K.Dollar
 		return	KClosure

 <|>	do	pTok K.Plus
 		return	KWitness

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
 =	do	var	<- pVarPlain
		(	do	-- VAR :: KIND
				pTok K.HasType
				kind	<- pKind
				return	(vNameDefaultN NameType var, kind)

		 <|>	-- VAR
                  	do	return (vNameDefaultN NameType var,
                        		kindOfVarSpace (Var.nameSpace var)))

 <?>    "pVar_withKind1"

-- Parse a body type with an optional context and constraint list
pType_bodyFetters :: Parser Type
pType_bodyFetters
 = do	mContext	<- Parsec.optionMaybe
			(Parsec.try $ do
				fs	<- pType_someContext
				pTok K.RightArrowEquals
				return	fs)

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
 =	pType_hsContext
 <|>	pType_context
 <?>	"pType_someContext"

-- Parse some class constraints written as a Disciple context
--	C1 => C2 => C3 ...
pType_context :: Parser [Fetter]
pType_context
 =	-- CONTEXT => CONTEXT ..
	(Parsec.try $ do
 		fs1	<- pType_classConstraint
 		pTok K.RightArrowEquals
		fs2	<- pType_context
		return	$ fs1 : fs2)

 <|>	(do	f	<- pType_classConstraint
		return	[f])
 <?>    "pType_context"

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
 =	do	con	<- liftM vNameW pCon
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
					return	$ (TBot KEffect, TBot KClosure, t2)

				-- TYPE -(EFF/CLO)> TYPE
			  <|>	do	pTok K.Dash
					pTok K.RBra
					typ	<- pTypeDashRBra t1
					return $ typ)

		case mRest of
			Just (eff, clo, t2)	-> return $ TFun t1 t2 eff clo
			_			-> return $ t1


pTypeDashRBra :: Type -> Parser (Type, Type, Type)
pTypeDashRBra t1
 =	-- EFF/CLO)> TYPE
	-- EFF)> TYPE
	do	eff	<- pEffect
		clo	<- Parsec.option (TBot KClosure) pClosure
		pTok K.RKet
		pTok K.AKet
		t2	<- pType_body
		return	$ (eff, clo, t2)

  <|>	-- CLO)> TYPE
	do	clo	<- pClosure
		pTok K.RKet
		pTok K.AKet
		t2	<- pType_body
		return $ (TBot KEffect, clo, t2)


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
	do	con	<- liftM vNameT $ pQualified pCon
 		args	<- Parsec.many pType_body1
		return	$ TData KNil con args

 <|>	do	t1	<- pType_body1
		Parsec.option t1
			(do	ts	<- Parsec.many1 pType_body1
				return	$ makeTApp (t1:ts))

 <?>    "pType_body2"

-- | Parse a type that can be used as an argument to a type constructor
pType_body1 :: Parser Type
pType_body1
 = 	-- VAR
 	-- If a variable had no namespace qualifier out the front the lexer will leave
	--	it in NameNothing. In this case we know its actually a type variable, so can
	--	set it in NameType.
 	do	var	<- liftM (vNameDefaultN NameType) $ pVarPlain
		return	$ TVar 	(kindOfSpace $ Var.nameSpace var) var

 <|>	-- ()
 	do	pTok K.Unit
		return	$ TData KValue (Var.primTUnit) []

 <|>	-- [ TYPE , .. ]
 	do	ts	<- pSParen $ Parsec.sepBy1 pType_body (pTok K.Comma)
		return	$ TData (KFun (KFun KRegion KValue) KValue)
				Var.primTList
				ts

 <|>	-- !Bot
	(Parsec.try $ do
		con	<- pConOfSpaceNamed [NameEffect] "!Bot"
		return	$ TBot KEffect)

 <|>	-- \$Bot
	(Parsec.try $ do
		con	<- pConOfSpaceNamed [NameClosure] "$Bot"
		return	$ TBot KClosure)

 <|>	-- CON
 	do	con	<- pQualified pCon
		return	$ TData KNil (vNameT con) []

 <|>	-- KIND _
 	(Parsec.try $ do
		k	<- pKind1
		pTok K.Underscore
		return	$ TWild k)

 <|>	pRParen pTypeBodyInRParen

 <?>    "pType_body1"


pTypeBodyInRParen :: Parser Type
pTypeBodyInRParen
 =	-- (VAR :: KIND)
	(Parsec.try $ do
		var	<- pVarPlain
		pTok K.HasType
		kind	<- pKind

		return	$ TVar kind $ vNameDefaultN NameType var)

 <|>	-- (CON :: KIND)
 	(Parsec.try $ do
		con	<- pQualified pCon
		pTok K.HasType
		kind	<- pKind

		return	$ TData kind (vNameT con) [])

 <|>	-- ( TYPE, TYPE .. )
	-- ( TYPE )
	do	ts	<- Parsec.sepBy1 pType_body (pTok K.Comma)
                if length ts == 1
                 then return	$ head ts
                 else return	$ TData (KFun (KFun KRegion KValue) KValue)
					(Var.primTTuple (length ts)) ts


-- Effect ------------------------------------------------------------------------------------------
-- | Parse an effect
pEffect :: Parser Type
pEffect
 = 	-- VAR
 	do	var	<- pVarPlainOfSpace [NameEffect]
		return $ TVar KEffect var

 <|>	-- !{ EFF; .. }
 	do	pTok	K.Bang
		effs	<- pCParen $ Parsec.sepEndBy1 pEffect pSemis
		return	$ TSum KEffect effs

 <|>	-- !SYNC
	do	var	<- pConOfSpaceNamed [NameEffect] "SYNC"
 		return	$ TTop KEffect

 <|>	-- !CON TYPE..
	do	con	<- pQualified $ pConOfSpace [NameEffect]
 		ts	<- Parsec.many pType_body1
		return	$ TEffect
				(vNameE con)
				ts
 <?>    "pEfect"


-- Closure -----------------------------------------------------------------------------------------
-- | Parse a closure
pClosure :: Parser Type
pClosure
  	-- VAR :  CLO
  	-- VAR :  TYPE
	-- VAR $> VAR
 =	(Parsec.try $ pQualified pVar >>= \var ->

	  	(pTok K.Colon >>= \_ ->
			-- VAR :  CLO
			(do	clo	<- pClosure
				return $ TFree var clo)

		  	-- VAR :  TYPE
		   <|>	(do	typ	<- pType
 				return	$ TFree var typ))

		-- VAR $> CLO
	  <|>	(pTok K.HoldsMono >>
	  		(do	var2	<- liftM (vNameDefaultN NameType) pVarPlain
				return	$ TDanger
					(TVar KRegion var)
					(TVar (kindOfSpace $ Var.nameSpace var2) var2))))


 <|>	-- \${ CLO ; .. }
 	do	pTok	K.Dollar
		clos	<- pCParen $ Parsec.sepEndBy1 pClosure pSemis
		return	$ TSum KClosure clos

 <|>	-- VAR
	do	var	<- pVarPlainOfSpace [NameClosure]
		return	$ TVar KClosure var
 <?>    "pClosure"

-- Fetter ------------------------------------------------------------------------------------------
-- | Parse a fetter
pFetter :: Parser Fetter
pFetter
 =  	-- CON TYPE..
	do	con	<- pQualified pCon
		ts	<- Parsec.many pType_body1
		return	$ FConstraint (vNameW con) ts


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
				return	$ TFun t1 t2 (TBot KEffect) (TBot KClosure))

 <?>    "pTypeOp"

pTypeOp1 :: Parser Type
pTypeOp1
 =	-- CON
	do	con	<- liftM vNameT $ pQualified pCon
		ts	<- Parsec.many pTypeOp1
		return	$ TData KNil con ts

