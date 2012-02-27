
module DDC.Core.Parser.Base
        ( Parser(..)
        , pWbCon
        , pModuleName
        , pQualName
        , pName
        , pCon
        , pVar
        , pLit)
where
import DDC.Base.Pretty
import DDC.Type.Parser                  (pTok)
import DDC.Core.Module
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


-- | Parse a module name.                               
-- TODO: handle hierarchical names.
--       reject hashes on end of name.
pModuleName :: Pretty n => Parser n ModuleName
pModuleName = P.pTokMaybe f
 where  f (KN (KCon n)) = Just $ ModuleName [renderPlain $ ppr n]
        f _             = Nothing


-- | Parse a qualified variable or constructor name.
pQualName :: Pretty n => Parser n (QualName n)
pQualName
 = do   mn      <- pModuleName
        pTok KDot
        n       <- pName
        return  $ QualName mn n


-- | Parse a constructor or variable name.
pName :: Parser n n
pName   = P.choice [pCon, pVar]


-- | Parse a constructor name.
pCon  :: Parser n n
pCon    = P.pTokMaybe f
 where  f (KN (KCon n)) = Just n
        f _             = Nothing


-- | Parse a variable name.
pVar :: Parser n n
pVar    = P.pTokMaybe f
 where  f (KN (KVar n)) = Just n
        f _             = Nothing


-- | Parse a literal
pLit :: Parser n n
pLit    = P.pTokMaybe f
 where  f (KN (KLit n)) = Just n
        f _             = Nothing

