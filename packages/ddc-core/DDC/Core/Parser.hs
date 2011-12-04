
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , PrimHandler (..)
        , pExp
        , pWitness)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Literal
import DDC.Base.Parser                  (pTokMaybe, pTokAs, pTok, (<?>))
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Parser        as T
import Control.Monad.Error


-- | Parser of core language tokens.
type Parser n a
        = P.Parser (Tok n) a

data PrimHandler p n
        = PrimHandler 
        { -- Construct a primitive literal.
          primMkLit     :: Literal -> Maybe p

          -- Construct a primitive operator.
        , primMkOp      :: n       -> Maybe p }


-- Expressions -----------------------------------------------------------------------------------
pExp    :: Ord n => PrimHandler p n
        -> Parser n (Exp () p n)
pExp = pExp2

pExp2   :: Ord n => PrimHandler p n 
        -> Parser n (Exp () p n)
pExp2 ph
 = P.choice
        -- Lambda abstractions
        [ do    pTok KBackSlash
                pTok KRoundBra
                bs      <- P.many1 T.pBinder
                pTok KColon
                t       <- T.pType
                pTok KRoundKet
                pTok KDot

                xBody   <- pExp2 ph

                return  $ foldr (XLam ()) xBody
                        $ map (\b -> T.makeBindFromBinder b t) bs

        , do    pExp1 ph ]

 <?> "an expression"


-- Applications
pExp1   :: Ord n => PrimHandler p n
        -> Parser n (Exp () p n)
pExp1 ph
  = do  (x:xs)  <- liftM concat $ P.many1 (pArgs ph)
        return  $ foldl (XApp ()) x xs

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgs   :: Ord n => PrimHandler p n
        -> Parser n [Exp () p n]
pArgs ph
 = P.choice
        -- {TYPE}
        [ do    pTok KBraceBra
                t       <- T.pType 
                pTok KBraceKet
                return  [XType t]

        -- {: TYPE0 TYPE0 ... :}
        , do    pTok KBraceColonBra
                ts      <- P.many1 T.pType0
                pTok KBraceColonKet
                return  $ map XType ts
        
        -- <WITNESS>
        , do    pTok KAngleBra
                w       <- pWitness
                pTok KAngleKet
                return  [XWitness w]
                
        -- <: WITNESS0 WITNESS0 ... :>
        , do    pTok KAngleColonBra
                ws      <- P.many1 pWitness0
                pTok KAngleColonKet
                return  $ map XWitness ws
                
        -- EXP0
        , do    x       <- pExp0 ph
                return  [x]
        ]
 <?> "a type type, witness or expression argument"


-- Atomics
pExp0   :: Ord n => PrimHandler p n
        -> Parser n (Exp () p n)
pExp0 ph
  = P.choice
        -- (EXP2)
        [ do    pTok KRoundBra
                t       <- pExp2 ph
                pTok KRoundKet
                return  $ t

        -- Primitive Literals
        , do    pLit ph 
        
        -- Primitive Operators
        , do    pPrimOp ph
        
        -- Named type constructors
        , do    con       <- pCon
                return  $ XCon () (UName con (T.tBot T.kData)) 

        -- Variables
        , do    var     <- pVar
                return  $ XVar () (UName var (T.tBot T.kData)) 
        ]

 <?> "a variable, constructor, or parenthesised type"

-- Primitives -------------------------------------------------------------------------------------
-- | Parse a primitive literal.
pLit    :: PrimHandler p n
        -> Parser n (Exp () p n)

pLit (PrimHandler mkLit _) 
        = pTokMaybe
        $ \k -> case k of 
                 KInteger i 
                  | Just p      <- mkLit (LInteger i)
                  -> Just $ XPrim () p
                 
                 _ -> Nothing


-- | Parse a primitive operator.
pPrimOp :: PrimHandler p n
        -> Parser n (Exp () p n)

pPrimOp (PrimHandler _ mkOp)
        = pTokMaybe
        $ \k -> case k of 
                 KVar n 
                  | Just p      <- mkOp n
                  -> Just $ XPrim () p
                 
                 _ -> Nothing

-- Witnesses -------------------------------------------------------------------------------------
-- | Top level parser for witnesses.
pWitness :: Ord n  => Parser n (Witness n)
pWitness = pWitness0

pWitness0 :: Ord n => Parser n (Witness n)
pWitness0 
 = P.choice
        -- Named witness constructors.
        [ do    wc     <- pWiCon
                return $ WCon wc ]
 <?> "a witness"

---------------------------------------------------------------------------------------------------
-- | Parse a builtin named `WiCon`
pWiCon :: Parser n WiCon
pWiCon  = pTokMaybe
        $ \k -> case k of
                 KWiConBuiltin wc -> Just wc
                 _                -> Nothing

-- | Parse a constructor name
pCon :: Parser n n
pCon    = pTokMaybe
        $ \k -> case k of
                 KCon n -> Just n
                 _      -> Nothing

-- | Parse a variable name
pVar :: Parser n n
pVar    = pTokMaybe
        $ \k -> case k of
                 KVar n -> Just n
                 _      -> Nothing
