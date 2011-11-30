
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pExp
        , pWitness)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Parser
import qualified DDC.Type.Compounds     as T


-- | Parser of core language tokens.
type Parser k n a
        = ParserG (Tokens k n) k n a


-- Expressions -----------------------------------------------------------------------------------
pExp :: Ord n => Parser k n (Exp () p n)
pExp = pExp1

pExp1 :: Ord n => Parser k n (Exp () p n)
pExp1
        = choice
        -- Lambda abstractions
        [ do    pTok tBackSlash
                pTok tRoundBra
                var     <- pVar
                -- TODO add type
                pTok tRoundKet
                pTok tDot
                xBody   <- pExp
                return  $ XLam () (BName var (T.tBot T.kData)) xBody

        , do    pExp0 ]


pExp0 :: Ord n => Parser k n (Exp () p n)
pExp0
        = choice
        -- Named type constructors
        [ do    var       <- pDaConUser
                return  $ XCon () (UName var (T.tBot T.kData)) 

        -- Variables
        , do    var     <- pVar
                return  $ XVar () (UName var (T.tBot T.kData)) ]


-- Witnesses -------------------------------------------------------------------------------------
-- | Top level parser for witnesses.
pWitness :: Ord n => Parser k n (Witness n)
pWitness = pWitness0

pWitness0 :: Ord n => Parser k n (Witness n)
pWitness0 
        = choice
        -- Named witness constructors.
        [ do    wc     <- pWiConBuiltin
                return $ WCon wc ]



---------------------------------------------------------------------------------------------------
-- | Parse a user defined named `TyCon`.
pDaConUser :: Parser k n n
pDaConUser      = pToken tDaConUser

-- | Parse a builtin named `WiCon`
pWiConBuiltin :: Parser k n WiCon
pWiConBuiltin   = pToken tWiConBuiltin

-- | Parse a variable.
pVar :: Parser k n n
pVar            = pToken tVar
