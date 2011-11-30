
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pExp
        , pWitness)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Parser                  (pTokMaybe, pTokAs, pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T


-- | Parser of core language tokens.
type Parser n a
        = P.Parser (Tok n) a


-- Expressions -----------------------------------------------------------------------------------
pExp :: Ord n => Parser n (Exp () p n)
pExp = pExp1

pExp1 :: Ord n => Parser n (Exp () p n)
pExp1
        = P.choice
        -- Lambda abstractions
        [ do    pTok KBackSlash
                pTok KRoundBra
                var     <- pVar
                -- TODO add type
                pTok KRoundKet
                pTok KDot
                xBody   <- pExp
                return  $ XLam () (BName var (T.tBot T.kData)) xBody

        , do    pExp0 ]


pExp0 :: Ord n => Parser n (Exp () p n)
pExp0
        = P.choice
        -- Named type constructors
        [ do    con       <- pCon
                return  $ XCon () (UName con (T.tBot T.kData)) 

        -- Variables
        , do    var     <- pVar
                return  $ XVar () (UName var (T.tBot T.kData)) ]


-- Witnesses -------------------------------------------------------------------------------------
-- | Top level parser for witnesses.
pWitness :: Ord n => Parser n (Witness n)
pWitness = pWitness0

pWitness0 :: Ord n => Parser n (Witness n)
pWitness0 
        = P.choice
        -- Named witness constructors.
        [ do    wc     <- pWiCon
                return $ WCon wc ]



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






