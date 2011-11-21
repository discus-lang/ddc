
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pTyConBuiltin
        , pTyConUser
        , pVar)  
where
import DDC.Core.Parser.Tokens
import DDC.Type.Exp
import DDC.Base.Parser


-- | Parser of core language tokens.
type Parser k n a
        = ParserG (Tokens k n) k n a



---------------------------------------------------------------------------------------------------
-- | Parse a builtin named tycon.
pTyConBuiltin :: Parser k n (TyCon n)
pTyConBuiltin   = pToken tTyConBuiltin

-- | Parse a user defined named tycon.
pTyConUser :: Parser k n (TyCon n)
pTyConUser      = pToken tTyConUser

-- | Parse a variable.
pVar :: Parser k n n
pVar            = pToken tVar
