
module DDC.Core.Parser.Base
        ( Parser(..)
        , pWbCon
        , pVar
        , pCon
        , pLit)
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import qualified DDC.Base.Parser        as P


-- | A parser of core language tokens.
type Parser n a
        = P.Parser (Tok n) a


-- | Parse a builtin named `WiCon`
pWbCon :: Parser n WbCon
pWbCon  = P.pTokMaybe f
 where  f (KA (KWbConBuiltin wb)) = Just wb
        f _                       = Nothing


-- | Parse a variable name
pVar :: Parser n n
pVar    = P.pTokMaybe f
 where  f (KN (KVar n)) = Just n
        f _             = Nothing


-- | Parse a constructor name
pCon :: Parser n n
pCon    = P.pTokMaybe f
 where  f (KN (KCon n)) = Just n
        f _             = Nothing


-- | Parse a literal
pLit :: Parser n n
pLit    = P.pTokMaybe f
 where  f (KN (KLit n)) = Just n
        f _             = Nothing

