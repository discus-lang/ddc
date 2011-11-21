
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pWitness)
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Parser


-- | Parser of core language tokens.
type Parser k n a
        = ParserG (Tokens k n) k n a


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
{-
-- | Parse a builtin named `TyCon`.
pTyConBuiltin :: Parser k n (TyCon n)
pTyConBuiltin   = pToken tTyConBuiltin

-- | Parse a user defined named `TyCon`.
pTyConUser :: Parser k n (TyCon n)
pTyConUser      = pToken tTyConUser
-}
-- | Parse a builtin named `WiCon`
pWiConBuiltin :: Parser k n WiCon
pWiConBuiltin   = pToken tWiConBuiltin
{-
-- | Parse a variable.
pVar :: Parser k n n
pVar            = pToken tVar
-}