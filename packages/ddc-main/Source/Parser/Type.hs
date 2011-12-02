module Source.Parser.Type
	( pSuper
	, pKind
	, pType, pType_body, pType_body1, pTypeOp)
where
import Source.Parser.Base
import Control.Monad
import Data.Maybe
import DDC.Type
import DDC.Var
import qualified Data.Map					as Map
import qualified Source.Token					as K
import qualified Shared.VarPrim					as Var
import qualified Text.ParserCombinators.Parsec.Combinator	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim		as Parsec


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
		
 <?>    "a super kind"	


-- Kind --------------------------------------------------------------------------------------------
pKind :: Parser Kind
pKind
 = 	-- KIND -> KIND
	do	k1	<- pKind1

	        Parsec.option k1
                 $ do	pTok K.RightArrow
			k2	<- pKind
			return $ KFun k1 k2

 <?>    "a kind"


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

 <|>	-- ( KIND )
	pRParen pKind

 <?>    "a simple kind"


-- Type --------------------------------------------------------------------------------------------
pType :: Parser Type
pType
 =  	-- forall VAR .. . TYPE
 	do	tok	<- pTok K.Forall
		vks	<- Parsec.many1 pVar_withKind
		pTok K.Dot
		body	<- pType_bodyFetters
		return	$ makeTForall_back 
				[(BVar v, k) | (v, k) <- vks ]
				body

 <|>	pType_bodyFetters
 <?>    "a type"


-- Parse a body type with an optional context and constraint list
pType_bodyFetters :: Parser Type
pType_bodyFetters
 = do	mContext	<- Parsec.optionMaybe
				(Parsec.try pType_someContext)

  	body		<- pType_body

 	mFetters	<- Parsec.optionMaybe
                	(do	pTok K.HasConstraint
				Parsec.sepBy1 pFetter (pTok K.Comma))

	-- NOTE: We put all the fetters in the crsOther field for now. They don't 
	--       have real uniqids, so they won't fit in a Data.Map yet.
	case concat $ maybeToList mContext ++ maybeToList mFetters of
		[]	-> return body
		fs	-> return $ TConstrain body 
				  $ Constraints Map.empty Map.empty fs	


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
					return	(tPure, tEmpty, t2)

				-- TYPE -(EFF/CLO)> TYPE
			  <|>	do	pTok K.Dash
					pTok K.RBra
					pTypeDashRBra t1
			  <?>	"function type")

		case mRest of
			Just (eff, clo, t2)	-> return $ makeTFun t1 t2 eff clo
			_			-> return t1


pTypeDashRBra :: Type -> Parser (Type, Type, Type)
pTypeDashRBra t1
 =	-- EFF/CLO)> TYPE
	-- EFF)> TYPE
	do	eff	<- pEffect
		clo	<- Parsec.option tEmpty pClosure1
		pTok K.RKet <?> "closing ')' for annotation on function constructor"
		pTok K.AKet <?> "'>' to finish the function constructor"
		t2	<- pType_body
		return	(eff, clo, t2)

  <|>	-- CLO)> TYPE
	do	clo	<- pClosure1
		pTok K.RKet <?> "closing ')' for annotation on function constructor"
		pTok K.AKet <?> "'>' to finish the function constructor"
		t2	<- pType_body
		return (tPure, clo, t2)
  <?> "function type"


pType_body3 :: Parser Type
pType_body3
 = pType_body2 >>= \t ->

	-- TYPE {read}
 	(do	tElab	<- pCParen
		    $	(do	pVarPlainNamed "read"
				return tElaborateRead)

		    <|>	(do	pVarPlainNamed "write"
				return tElaborateWrite)

		    <|>	(do	pVarPlainNamed "modify"
				return tElaborateModify)

		return	$ TApp tElab t)

	-- TYPE
   <|>		return	t
   <?>      "a possibly annotated type expression"


-- | Parse a type that can be used as an argument to a function constructor
pType_body2 :: Parser Type
pType_body2
 =	-- CON TYPE..
	do	t1	<- pTyCon
 		args	<- Parsec.many pType_body1
		return	$ makeTApp t1 args
		
 <|>	do	t1	<- pType_body1
		Parsec.option t1
			(do	ts	<- Parsec.many1 pType_body1
				return	$ makeTApp t1 ts)

 <?>    "a type"


-- | Parse a type that can be used as an argument to a type constructor
pType_body1 :: Parser Type
pType_body1
 = 	-- ()
 	do	pTok K.Unit
		return	$ makeTData Var.primTUnit kValue []

 <|>	-- [ TYPE , .. ]
 	do	ts	<- pSParen $ Parsec.sepBy1 pType_body (pTok K.Comma)
		return	$ makeTData 
				Var.primTList
				(KFun (KFun kRegion kValue) kValue)
				ts

 	-- VAR
 	-- If a variable had no namespace qualifier out the front the lexer will leave
	--	it in NameNothing. In this case we know its actually a type variable, so can
	--	set it in NameType.
 <|>	do	var	<- liftM (vNameDefaultN NameType) $ pQualified pVarPlain
		return	$ TVar 	(let Just k = kindOfSpace $ varNameSpace var in k) $ UVar var
		
 <|>	pRParen pParenTypeBody

 <|>	-- \*Bot / %Bot / !Bot / \$Bot
	pConBottom

 <|>	-- CON
 	do	con	<- pTyCon
		return	$ con

 <?>    "a simple type"


pParenTypeBody :: Parser Type
pParenTypeBody
 =	-- (VAR :: KIND)
	Parsec.try
	  (do	var	<- pOfSpace NameType pVarPlain
		pTok K.HasTypeMatch
		kind	<- pKind

		return	$ TVar kind $ UVar var)

 <|>	-- (CON :: KIND)
	Parsec.try
	  (do	con	<- pOfSpace NameType $ pQualified pCon
		pTok K.HasTypeMatch
		kind	<- pKind

		return	$ makeTData con kind [])

 <|>	-- ( TYPE, TYPE .. )
	-- ( TYPE )
	do	ts	<- Parsec.sepBy1 pType_body (pTok K.Comma)
		case ts of
                  [hts] -> return hts
                  _ -> return	$ makeTData 
				(Var.primTTuple (length ts))
				(KFun (KFun kRegion kValue) kValue)
				ts

 <?> 	"parenthesised body type "


-- Type Class Contexts -----------------------------------------------------------------------------
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
	pRParen $ Parsec.sepBy1 pType_classConstraint (pTok K.Comma)


-- Parse a single type class constraint
--	Con Type*
pType_classConstraint :: Parser Fetter
pType_classConstraint
 =	do	con	<- pOfSpace NameClass pCon
	 	ts	<- Parsec.many1 pType_body1
		return	$ FConstraint con ts


-- Effect ------------------------------------------------------------------------------------------
-- | Parse an effect
pEffect :: Parser Type
pEffect
  = do	-- EFF + EFF + ...
 	effs	<- Parsec.sepBy1 pEffect1 (pTok K.Plus)
	case effs of
	 [eff]	-> return eff
	 _	-> return $ TSum kEffect effs


pEffect1 :: Parser Type
  = 	-- VAR
 	do	var	<- pVarPlainOfSpace [NameEffect]
		return $ TVar kEffect $ UVar var

 <|>	-- !CON TYPE..
	do	t1	<- pTyCon
 		ts	<- Parsec.many pType_body1
		return	$ makeTApp t1 ts

 <|>	-- !0
	do	pTok K.Bang
		pZero
		return	$ tBot kEffect

 <?>    "a simple effect"


-- Closure -----------------------------------------------------------------------------------------
-- | Parse a closure
pClosure :: Parser Type
pClosure
 = do	-- CLO + CLO + ...
	clos	<- Parsec.sepBy1 pClosure1 (pTok K.Plus)
	case clos of
	 [clo]	-> return clo
	 _	-> return $ TSum kClosure clos
		

pClosure1 :: Parser Type
pClosure1

  	-- \${ VAR :  CLO  }
  	-- \${ VAR :  TYPE }
 = do 	pTok K.Dollar
	pCParen $ do
		var <- pQualified pVar
		let varN	= vNameDefaultN NameValue var
		pTok K.Colon
			
		-- VAR : CLO
		(do	clo		<- pClosure
			let Just clo'	= makeTFree varN clo
			return clo'

	 	 -- VAR : TYPE
		 <|> do typ	<- pType
			return	$ TApp (tFreeType varN) typ)

 <|> Parsec.try	-- VAR $> VAR
	(do	var1	<- pVarPlain
		pTok K.HoldsMono
		var2	<- pVarPlain
		let var1N	= vNameDefaultN NameType var1
	 	let var2N	= vNameDefaultN NameType var2
		return	$ makeTDanger
				(TVar kRegion $ UVar var1N)
				(TVar (let Just k = kindOfSpace $ varNameSpace var2N in k) $ UVar var2N))

 <|>	-- VAR
	do	var	<- pVarPlainOfSpace [NameClosure]
		return	$ TVar kClosure $ UVar var

 <?>    "a closure"


-- Fetter ------------------------------------------------------------------------------------------
-- | Parse a fetter
pFetter :: Parser Fetter
pFetter
 =  	-- CON TYPE..
	do	con	<- pOfSpace NameClass $ pQualified pCon
		ts	<- Parsec.many pType_body1
		return	$ FConstraint con ts

 -- NOTE: The following cases are duplicated to get better error messages.
 --       If we see an effect var on the left of the constraint we're expecting
 --       an effect expression on the right, and not a closure.
 <|>	-- VAR =  EFF
	-- VAR :> EFF
	(pVarPlainOfSpace [NameEffect] >>= \var ->
		-- VAR = EFF
 		(do	pTok K.Equals
			eff	<- pEffect <?> "an effect constraint for " ++ quotVar var
			return	$ FWhere (TVar kEffect $ UVar var) eff)

		-- VAR :> EFF
	  <|>	(do	pTok K.HasTypeMore 
			eff	<- pEffect <?> "an effect constraint for " ++ quotVar var
			return	$ FMore  (TVar kEffect $ UVar var) eff)

	  <?>	"an '=' or ':>' and the rest of the effect constraint for " ++ quotVar var)

 <|>	-- VAR =  CLO
	-- VAR :> CLO
	(pVarPlainOfSpace [NameClosure] >>= \var ->
		-- VAR = CLO
 		(do	pTok K.Equals
			clo	<- pClosure <?> "a closure constraint for " ++ quotVar var
			return	$ FWhere (TVar kClosure $ UVar var) clo)

		-- VAR :> CLO
	  <|>	(do	pTok K.HasTypeMore
			clo	<- pClosure <?> "a closure constraint for " ++ quotVar var
			return	$ FMore (TVar kClosure $ UVar var) clo)
	
	  <?>	"an '=' or ':>' and the rest of the closure constraint for " ++ quotVar var)

 <?>    "a type constraint"


-- Bottom Types -----------------------------------------------------------------------------------
pConBottom :: Parser Type
pConBottom
 =	do	pTok	K.Star
		pZero
		return	$ tBot kValue

 <|>	do	pTok	K.Percent
		pZero
        	return $ tBot kRegion

 <|>	do	pTok	K.Bang
		pZero
        	return $ tBot kEffect

 <|>	do	pTok	K.Dollar
		pZero
        	return $ tBot kClosure

 <?>	"a zero/bottom type"


-- TypeOp ------------------------------------------------------------------------------------------
-- | Parse an operational type.
pTypeOp :: Parser Type
pTypeOp
 = do	t1	<- pTypeOp1
	Parsec.option t1
	 (do	pTok K.RightArrow
		t2	<- pTypeOp
		return	$ makeTFun t1 t2 tPure tEmpty)

 <?>    "an operational type"

pTypeOp1 :: Parser Type
pTypeOp1
 = do	-- CON
	con	<- pOfSpace NameType $ pQualified pCon
	ts	<- Parsec.many pTypeOp1
	return	$ makeTData con KNil ts


-- Type Constructors ------------------------------------------------------------------------------
-- | Parse a type constructor.
pTyCon :: Parser Type
pTyCon 	
 = do	con	<- pQualified pCon
	case varNameSpace con of
		NameEffect 	-> pTyCon_effect con
		_		-> return $ TCon (TyConData con kValue Nothing)
		
pTyCon_effect con
 = case varName con of
	"Read"		-> return tRead
	"ReadH"		-> return tHeadRead
	"ReadT"		-> return tDeepRead
	"Write"		-> return tWrite
	"WriteT"	-> return tDeepWrite
	_		-> return $ TCon (TyConEffect (TyConEffectTop con) kEffect)


-- Vars with Kinds --------------------------------------------------------------------------------
-- Parse a quantified variable, with optional kind
pVar_withKind :: Parser (Var, Kind)
pVar_withKind
 = 	pRParen pVar_withKind1
 <|>	pVar_withKind1
 <?>    "a variable with it's kind"


pVar_withKind1 :: Parser (Var, Kind)
pVar_withKind1
 = do	var	<- liftM (vNameDefaultN NameType) pVarPlain
	(	do	-- VAR :: KIND
			pTok K.HasTypeMatch	<?> "a '::' and the kind for '" ++ varName var ++ "'"
			kind	<- pKind	<?> "a kind"
			return	(var, kind)

	 <|>	-- VAR
		return (var, kindOfVarSpace (varNameSpace var)))

 <?>    "a variable with it's kind"

