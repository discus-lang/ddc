
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pExp
        , pWitness)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Parser                  ((<?>))
import DDC.Type.Parser                  (pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Parser        as T
import Control.Monad.Error


-- | Parser of core language tokens.
type Parser n a
        = P.Parser (Tok n) a


-- Expressions -----------------------------------------------------------------------------------
pExp    :: Ord n => Parser n (Exp () n)
pExp = pExp2

pExp2   :: Ord n => Parser n (Exp () n)
pExp2 
 = P.choice
        -- Lambda abstractions
        [ do    pTok KBackSlash
                pTok KRoundBra
                bs      <- P.many1 T.pBinder
                pTok KColon
                t       <- T.pType
                pTok KRoundKet
                pTok KDot

                xBody   <- pExp2

                return  $ foldr (XLam ()) xBody
                        $ map (\b -> T.makeBindFromBinder b t) bs

        , do    pExp1 ]

 <?> "an expression"


-- Applications
pExp1   :: Ord n => Parser n (Exp () n)
pExp1 
  = do  (x:xs)  <- liftM concat $ P.many1 pArgs
        return  $ foldl (XApp ()) x xs

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgs   :: Ord n => Parser n [Exp () n]
pArgs 
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
        , do    x       <- pExp0
                return  [x]
        ]
 <?> "a type type, witness or expression argument"


-- Atomics
pExp0   :: Ord n => Parser n (Exp () n)
pExp0 
  = P.choice
        -- (EXP2)
        [ do    pTok KRoundBra
                t       <- pExp2
                pTok KRoundKet
                return  $ t
        
        -- Named type constructors
        , do    con       <- pCon
                return  $ XCon () (UName con (T.tBot T.kData)) 

        -- Debruijn indices
        , do    i       <-T.pIndex
                return  $ XVar () (UIx   i  (T.tBot T.kData))

        -- Variables
        , do    var     <- pVar
                return  $ XVar () (UName var (T.tBot T.kData)) 
        ]

 <?> "a variable, constructor, or parenthesised type"


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
pWiCon  = P.pTokMaybe
        $ \k -> case k of
                 KA (KWiConBuiltin wc) -> Just wc
                 _                     -> Nothing

-- | Parse a constructor name
pCon :: Parser n n
pCon    = P.pTokMaybe
        $ \k -> case k of
                 KN (KCon n) -> Just n
                 _           -> Nothing

-- | Parse a variable name
pVar :: Parser n n
pVar    = P.pTokMaybe
        $ \k -> case k of
                 KN (KVar n) -> Just n
                 _           -> Nothing



