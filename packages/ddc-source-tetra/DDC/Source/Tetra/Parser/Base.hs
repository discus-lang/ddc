{-# LANGUAGE ExplicitNamespaces #-}
module DDC.Source.Tetra.Parser.Base
        ( type Parser
        , (<?>), SourcePos(..)

          -- * Generic Token parsers
        , pTok, pTokSP

          -- * Term Variables
        , pBindNameSP
        , pBoundName,      pBoundNameSP
        , pBoundIxSP
        , pBoundNameOpVarSP
        , pBoundNameOpSP

          -- * DaCons
        , pDaConBindName
        , pDaConBoundName, pDaConBoundNameSP
        , pDaConBoundLit,  pDaConBoundLitSP

          -- * Primitive Operators
        , pPrimValSP)
where
import DDC.Source.Tetra.Exp.Bind
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Prim
import DDC.Core.Lexer.Tokens
import DDC.Base.Parser                  ((<?>))
import DDC.Base.Parser                  (SourcePos(..))
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as Text


import DDC.Core.Parser
        ( pTok,         pTokSP
        , pIndexSP)

type Parser a = P.Parser (Tok Name) a


-- Type and Term Variables ----------------------------------------------------
-- | Parse a binding occurrence of a named variable.
pBindNameSP  :: Parser (Bind, SourcePos)
pBindNameSP = P.pTokMaybeSP f <?> "a variable"
 where  f (KN (KVar (NameVar s)))       = Just (BName s)
        f _                             = Nothing


-- | Parse a named term variable.
pBoundName :: Parser Bound
pBoundName = P.pTokMaybe f <?> "a variable"
 where  f (KN (KVar (NameVar s)))       = Just (UName s)
        f _                             = Nothing


-- | Parse a named term variable.
pBoundNameSP :: Parser (Bound, SourcePos)
pBoundNameSP = P.pTokMaybeSP f <?> "a variable"
 where  f (KN (KVar (NameVar s)))       = Just (UName s)
        f _                             = Nothing


-- | Parse an indexed term variable.
pBoundIxSP   :: Parser (Bound, SourcePos)
pBoundIxSP 
 = do   (i, sp) <- pIndexSP
        return  $ (UIx i, sp)


-- | Parse an infix operator used as a variable.
pBoundNameOpSP :: Parser (Bound, SourcePos)
pBoundNameOpSP = P.pTokMaybeSP f
 where  f (KA (KOp s))                  = Just (UName (Text.pack s))
        f _                             = Nothing


-- | Parse an infix operator used as a variable.
pBoundNameOpVarSP :: Parser (Bound, SourcePos)
pBoundNameOpVarSP = P.pTokMaybeSP f
 where  f (KA (KOpVar s))               = Just (UName (Text.pack s))
        f _                             = Nothing


-- DaCons ---------------------------------------------------------------------
-- | Parse a binding occurrence of a data constructor name.
pDaConBindName :: Parser DaConBind
pDaConBindName = P.pTokMaybe f
 where  f (KN (KCon (NameCon n)))       = Just (DaConBindName n)
        f _                             = Nothing


-- | Parse a bound occurrence of a data constructor name.
pDaConBoundName :: Parser DaConBound
pDaConBoundName = P.pTokMaybe f
 where  f (KN (KCon (NameCon n)))       = Just (DaConBoundName n)
        f _                             = Nothing


-- | Parse a constructor name.
pDaConBoundNameSP :: Parser (DaConBound, SourcePos)
pDaConBoundNameSP = P.pTokMaybeSP f
 where  f (KN (KCon (NameCon n)))       = Just (DaConBoundName n)
        f _                             = Nothing

-- | Parse a literal.
pDaConBoundLit :: Parser DaConBound
pDaConBoundLit = P.pTokMaybe f
 where  f (KN (KLit (NamePrimValLit n))) = Just (DaConBoundLit n)
        f (KA (KString tx))              = Just (DaConBoundLit (PrimLitTextLit tx))
        f _                              = Nothing


-- | Parse a literal, with source position.
pDaConBoundLitSP :: Parser (DaConBound, SourcePos)
pDaConBoundLitSP = P.pTokMaybeSP f
 where  f (KN (KLit (NamePrimValLit n))) = Just (DaConBoundLit n)
        f (KA (KString tx))              = Just (DaConBoundLit (PrimLitTextLit tx))
        f _                              = Nothing


-- Primitive Values -----------------------------------------------------------
pPrimValSP :: Parser (PrimVal, SourcePos)
pPrimValSP =  P.pTokMaybeSP f <?> "a primitive operator"
 where  f (KN (KVar (NamePrimValOp p))) = Just p
        f _                             = Nothing


