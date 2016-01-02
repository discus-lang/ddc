
module DDC.Source.Tetra.Parser.Atom
        ( pPrimValSP
        , pVarStringSP)
where
import DDC.Source.Tetra.Prim
import DDC.Core.Lexer.Tokens
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P

import DDC.Core.Parser
        (Parser)


-- | Parse a variable, with source position.
pPrimValSP :: Parser Name (PrimVal, SourcePos)
pPrimValSP =  P.pTokMaybeSP f
        <?> "a variable"
 where  f (KN (KVar (NameVal p)))      = Just p
        f _                            = Nothing


-- | Parse a variable, with source position.
pVarStringSP :: Parser Name (String, SourcePos)
pVarStringSP =  P.pTokMaybeSP f
        <?> "a variable"
 where  f (KN (KVar (NameVar s)))       = Just s
        f _                             = Nothing
