
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
 	-- overlaps with kind1
	(Parsec.try $ do
		k1	<- pKind1
 		pTok K.RightArrow
		k2	<- pKind
		return	$ KFun k1 k2)
		
 <|>	pKind1
 
pKind1 :: Parser Kind
pKind1 
 = 	do	pTok K.Star
 		return	KData
		
 <|>	do	pTok K.Percent
 		return	KRegion
		
 <|>	do	pTok K.Bang
 		return	KEffect
		
 <|>	do	pTok K.Dollar
 		return	KClosure
		
 <|>	do	pTok K.Plus
 		return	KFetter

 <|>	-- ( KIND )
 	do	pRParen pKind
 			

-- Type --------------------------------------------------------------------------------------------

-- Parse a type.
pType :: Parser Type
pType
 = 
 	-- elaborate TYPE
	do	pTok K.Elaborate
		t	<- pType
		return	$ TElaborate t
 
 	-- forall VAR .. . TYPE
 <|>	do	tok	<- pTok K.Forall
		vks	<- Parsec.many1 pVar_withKind
		pTok K.Dot
		body	<- pType_bodyFetters
		return	$ TForall vks body

 <|>	pType_bodyFetters	 
	
		
-- Parse a quantified variable, with optional kind
pVar_withKind :: Parser (Var, Kind)
pVar_withKind
 = 	pRParen pVar_withKind1
 <|>	pVar_withKind1
  		 
pVar_withKind1 :: Parser (Var, Kind)
pVar_withKind1
 =	-- VAR :: KIND
 	-- overlaps with VAR
 	(Parsec.try $ do
 		var	<- pVarPlain
		pTok K.HasType
		kind	<- pKind
		return	( vNameDefaultN NameType var
			, kind) )
 	-- VAR
 <|> 	do	var	<- pVarPlain
 		return	( vNameDefaultN NameType var
			, kindOfVarSpace (Var.nameSpace var) )
	

-- Parse a body type with an optional context and fetter list
pType_bodyFetters :: Parser Type
pType_bodyFetters
 = do	mContext	<- (Parsec.try $ do
					fs	<- pType_context
					pTok K.RightArrowEquals
					return $ Just fs)
			  <|> return Nothing
			
  	body		<- pType_body

 	mFetters	<- (Parsec.try $ do
				pTok K.HasConstraint
				fetters	<- Parsec.sepBy1 pFetter (pTok K.Comma)
				return $ Just fetters)
			 <|> return Nothing
	
	case concat $ maybeToList mContext ++ maybeToList mFetters of
		[]	-> return body
		fs	-> return $ TFetters fs body	
	
	
-- Parse some fetters written as a Haskell style type context
pType_context :: Parser [Fetter]
pType_context
	-- CONTEXT => CONTEXT ..
 = 	(Parsec.try $ do 	
 		fs1	<- pType_context1
 		pTok K.RightArrowEquals
		fs2	<- pType_context1
		return	$ fs1 ++ fs2)

 <|>	pType_context1
 

pType_context1
 = 	-- (CONTEXT, ..)
 	do	cs	<- pRParen $ Parsec.sepBy1 pType_context1 (pTok K.Comma)
 		return	(concat cs)

	-- CON TYPE..  
 <|> 	do	con	<- liftM vNameW pCon
	 	ts	<- Parsec.many1 pType_body1
		return	[FConstraint con ts]
	


-- Parse a body type (without a forall or fetters)
pType_body :: Parser Type
pType_body
 = 	-- TYPE -> TYPE
	-- overlaps with the rest
 	(Parsec.try $ do
		t1	<- pType_body2
		pTok K.RightArrow
		t2	<- pType_body
		return	$ TFun t1 t2 (TBot KEffect) (TBot KClosure))

 <|>	-- TYPE -(EFF/CLO)> TYPE
 	(Parsec.try $ do
		t1	<- pType_body2
		pTok K.Dash
		effclo	<- pRParen (Parsec.try pEffect <|> pClosure)
		pTok K.AKet
		t2	<- pType_body
		
		case kindOfType effclo of
			KEffect	 -> return $ TFun t1 t2 effclo (TBot KClosure)
			KClosure -> return $ TFun t1 t2 (TBot KEffect) effclo)
		
 <|>	-- TYPE (EFF CLO) TYPE
 	(Parsec.try $ do
		t1	<- pType_body2
		pTok K.Dash
		pTok K.RBra
		eff	<- pEffect
		clo	<- pClosure
		pTok K.RKet
		pTok K.AKet
		t2	<- pType_body
		
		return	$ TFun t1 t2 eff clo)

 <|>	-- TYPE
	pType_body2


-- | Parse a type that can be used as an argument to a function constructor
pType_body2 :: Parser Type
pType_body2
 =	-- mutable TYPE
 	do	pTok K.Mutable
		t	<- pType_body2
		return	$ TMutable t
  
 	-- CON TYPE..
 <|>	do	con	<- liftM vNameT $ pQualified pCon
 		args	<- Parsec.many pType_body1
		return	$ TData con args

 <|>	(Parsec.try $ do
 		t1	<- pType_body1
 		ts	<- Parsec.many1 pType_body1
		return	$ makeTApp (t1:ts))

 <|>	pType_body1


-- | Parse a type that can be used as an argument to a type constructor
pType_body1 :: Parser Type
pType_body1
 = 	-- VAR
 	-- If a variable had no namespace qualifier out the front the lexer will leave
	--	it in NameNothing. In this case we know its actually a type variable, so can 
	--	set it in NameType.
 	do	var	<- liftM (vNameDefaultN NameType) $ pVarPlain
		return	$ TVar 	(kindOfSpace $ Var.nameSpace var)
				var

 <|>	-- CON
 	do	con	<- liftM vNameT $ pQualified pCon
		return	$ TData con []
	
 <|>	-- ()
 	do	pTok K.Unit
		return	$ TData (Var.primTUnit) []

 <|>	-- KIND _
 	(Parsec.try $ do
		do	k	<- pKind1 
			pTok K.Underscore
			return	$ TWild k)
 	
 <|>	-- [ TYPE , .. ]
 	do	ts	<- pSParen $ Parsec.sepBy1 pType_body (pTok K.Comma)
		return	$ TData Var.primTList ts

 <|>	-- ( TYPE, TYPE .. )
 	-- overlaps with (TYPE)
	(Parsec.try $ do
		tok	<- pTok K.RBra
		t1	<- pType_body
		pTok K.Comma
		ts	<- Parsec.sepBy1 pType_body (pTok K.Comma)
		pTok K.RKet
		return	$ TData (Var.primTTuple (length (t1:ts))) (t1 : ts))

 <|>	-- ( TYPE )
 	pRParen pType_body


-- Effect ------------------------------------------------------------------------------------------
-- | Parse an effect
pEffect :: Parser Type
pEffect
 = 	-- VAR
 	do	var	<- pVarPlainOfSpace [NameEffect]
		return $ TVar KEffect var
		
	-- !CON VAR..
 <|>	do	con	<- pQualified $ pConOfSpace [NameEffect]
 		vars	<- Parsec.many (liftM (vNameDefaultN NameType) pVarPlain)
		return	$ TEffect 
				(vNameE con) 
				(map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vars)

 <|>	-- !{ EFF; .. }
 	do	pTok	K.Bang
		effs	<- pCParen $ Parsec.sepEndBy1 pEffect pSemis
		return	$ TSum KEffect effs
		
 
-- Closure -----------------------------------------------------------------------------------------
-- | Parse a closure
pClosure :: Parser Type
pClosure
 = 	-- CLO \ VAR
 	(Parsec.try $ do
 		clo1	<- pClosure1
		pTok K.BackSlash
		var	<- pVar
		return	$ TMask KClosure clo1 (TTag var))
 
  	-- VAR : CLO
 <|>	(Parsec.try $ do
		var	<- pQualified pVar
 		pTok K.Colon
		clo	<- pClosure
		return	$ TFree var clo)

  	-- VAR : TYPE
 <|>	(Parsec.try $ do
		var	<- pQualified pVar
 		pTok K.Colon
		t	<- pType
		return	$ TFree var t)

 <|>	-- VAR $> VAR
 	(Parsec.try $ do
		var1	<- pVarPlainOfSpace [NameRegion]
		pTok K.HoldsMono
		var	<- liftM (vNameDefaultN NameType) pVarPlain
		return	$ TDanger 
				(TVar KRegion var1) 
				(TVar (kindOfSpace $ Var.nameSpace var) var))
 	
 <|>	pClosure1

pClosure1 :: Parser Type
pClosure1
 =	-- ${ CLO ; .. }
 	do	pTok	K.Dollar
		clos	<- pCParen $ Parsec.sepEndBy1 pClosure pSemis
		return	$ TSum KClosure clos

	-- VAR
 <|> 	do	var	<- pVarPlainOfSpace [NameClosure]
		return	$ TVar KClosure var
 
	
-- Fetter ------------------------------------------------------------------------------------------
-- | Parse a fetter
pFetter :: Parser Fetter
pFetter
 = 
 	-- CON TYPE.. 
	do	con	<- pQualified pCon
		ts	<- Parsec.many pType_body1
		return	$ FConstraint (vNameW con) ts
		
 <|>	-- VAR = EFFECT/CLOSURE
 	-- overlaps with VAR :> EFFECT/CLOSURE
	(Parsec.try $ do
		var	<- pVarPlainOfSpace [NameEffect, NameClosure]
		pTok K.Equals
		
		effClo	<- Parsec.try pEffect <|> pClosure
		return	$ FLet 	(TVar (kindOfSpace $ Var.nameSpace var) var)
				effClo)

 <|>	-- VAR :> EFFECT/CLOSURE
 	do	var	<- pVarPlainOfSpace [NameEffect, NameClosure]
		pTok K.IsSuptypeOf
		
		effClo	<- Parsec.try pEffect <|> pClosure
		return	$ FMore (TVar (kindOfSpace $ Var.nameSpace var) var)
				effClo
		

-- TypeOp ------------------------------------------------------------------------------------------

-- Parse an operational type
pTypeOp :: Parser Type
pTypeOp
 =	(Parsec.try $ do
 		t1	<- pTypeOp1
		pTok K.RightArrow
		t2	<- pTypeOp
		return	$ TFun t1 t2 (TBot KEffect) (TBot KClosure))
 <|>	pTypeOp1
 
pTypeOp1 :: Parser Type
pTypeOp1
 = 	-- CON
 	do	con	<- liftM vNameT $ pQualified pCon
		ts	<- Parsec.many pTypeOp1
		return	$ TData con ts
		
