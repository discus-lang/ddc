{-# LANGUAGE ExplicitNamespaces #-}
module DDC.Source.Tetra.Parser.Base
        ( type Parser
        , type SP
        , (<?>), SourcePos(..)

          -- * Generic Token parsers
        , pSym, pKey
        , pTok, pTokSP

          -- * Term Variables
        , pBindNameSP
        , pBoundName,           pBoundNameSP
        , pBoundIxSP
        , pBoundNameOpVarSP
        , pBoundNameOpSP
        , pVarNameSP

          -- * TyCons
        , pTyConBindName,       pTyConBindNameSP

          -- * DaCons
        , pDaConBindName
        , pDaConBoundName,      pDaConBoundNameSP
        , pDaConBoundLit,       pDaConBoundLitSP

          -- * Primitive Operators
        , pPrimValSP)
where
import DDC.Source.Tetra.Exp.Bind        hiding (Name)
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Lexer
import DDC.Core.Lexer.Tokens
import DDC.Control.Parser               ((<?>))
import DDC.Control.Parser               (SourcePos(..))
import Data.Text                        (Text)
import qualified DDC.Control.Parser     as P
import qualified Data.Text              as Text


import DDC.Core.Parser
        ( pSym, pKey
        , pTok, pTokSP
        , pIndexSP)

type Parser a
        = P.Parser (Token Name) a

type SP = SourcePos

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


-- | Parse a variable-like name as a flat string.
pVarNameSP :: Parser (Text, SourcePos)
pVarNameSP  =  P.pTokMaybeSP f <?> "a name"
 where  f (KN (KVar (NameVar s)))       = Just s
        f _                             = Nothing


-- TyCons ---------------------------------------------------------------------
-- | Parse a binding occurrences of a type constructor name.
pTyConBindName :: Parser TyConBind
pTyConBindName = P.pTokMaybe f
 where  f (KN (KCon (NameCon n)))       = Just (TyConBindName n)
        f _                             = Nothing


-- | Parse a binding occurrences of a type constructor name.
pTyConBindNameSP :: Parser (TyConBind, SourcePos)
pTyConBindNameSP = P.pTokMaybeSP f
 where  f (KN (KCon (NameCon n)))       = Just (TyConBindName n)
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
--    These are numeric literals, string literals,
--    and special constructors like 'True' and 'False'.
pDaConBoundLit :: Parser DaConBound
pDaConBoundLit = P.pTokMaybe f
 where  f (KA (KLiteral lit False))      = fmap DaConBoundLit (primLitOfLiteral lit)
        f (KN (KCon (NamePrimValLit n))) = Just (DaConBoundLit n)
        f _                              = Nothing


-- | Parse a literal, with source position.
--    These are numeric literals, string literals,
--    and special constructors like 'True' and 'False'.
pDaConBoundLitSP :: Parser (DaConBound, SourcePos)
pDaConBoundLitSP = P.pTokMaybeSP f
 where  f (KA (KLiteral lit False))      = fmap DaConBoundLit (primLitOfLiteral lit)
        f (KN (KCon (NamePrimValLit n))) = Just (DaConBoundLit n)
        f _                              = Nothing


-- Primitive Values -----------------------------------------------------------
pPrimValSP :: Parser (PrimVal, SourcePos)
pPrimValSP =  P.pTokMaybeSP f <?> "a primitive operator"
 where  f (KN (KVar (NamePrimValOp p))) = Just p
        f _                             = Nothing


