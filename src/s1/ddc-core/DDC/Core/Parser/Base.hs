
module DDC.Core.Parser.Base
        ( Parser
        , pModuleName
        , pQualName
        , pName
        , pCon,         pConSP
        , pLit,         pLitSP
        , pIndex,       pIndexSP
        , pVar,         pVarSP
        , pVarName,     pVarNamedSP
        , pKey,         pSym
        , pTok,         pTokSP
        , pTokAs,       pTokAsSP
        , pOpSP
        , pOpVarSP
        , pPragmaSP)
where
import DDC.Data.Pretty
import DDC.Core.Module
import DDC.Core.Lexer.Tokens
import DDC.Control.Parser               ((<?>), SourcePos)
import Data.Text                        (Text)
import qualified DDC.Control.Parser     as P
import qualified Data.Text              as Text


-- | A parser of core language tokens.
type Parser n a
        = P.Parser (Token n) a


-- | Parse a module name.                               
pModuleName :: Pretty n => Parser n ModuleName
pModuleName 
 = do   ms      <- P.sepBy1 pModuleName1 (pTok (KSymbol SDot))
        return  $  ModuleName 
                $  concat
                $  map (\(ModuleName ss) -> ss) ms


-- | Parse a single component module name.
pModuleName1 :: Pretty n => Parser n ModuleName
pModuleName1 = P.pTokMaybe f
 where  f (KN (KCon n))           = Just $ ModuleName [ renderPlain $ ppr n ]

        -- These names are lexed as constructors
        -- but can be part of a module name.
        f (KA (KBuiltin (BSoCon c)))  = Just $ ModuleName [ renderPlain $ ppr c ]
        f (KA (KBuiltin (BKiCon c)))  = Just $ ModuleName [ renderPlain $ ppr c ]
        f (KA (KBuiltin (BTwCon c)))  = Just $ ModuleName [ renderPlain $ ppr c ]
        f (KA (KBuiltin (BTcCon c)))  = Just $ ModuleName [ renderPlain $ ppr c ]
        f _                       = Nothing


-- | Parse a qualified variable or constructor name.
pQualName :: Pretty n => Parser n (QualName n)
pQualName
 = do   mn      <- pModuleName
        pTok    (KSymbol SDot)
        n       <- pName
        return  $ QualName mn n


-- | Parse a constructor or variable name.
pName :: Parser n n
pName   = P.choice [pCon, pVar]


-- | Parse a constructor name.
pCon    :: Parser n n
pCon    = P.pTokMaybe f
 where  f (KN (KCon n)) = Just n
        f _             = Nothing


-- | Parse a constructor name.
pConSP    :: Parser n (n, SourcePos)
pConSP    = P.pTokMaybeSP f
 where  f (KN (KCon n)) = Just n
        f _             = Nothing


-- | Parse a literal.
pLit :: Parser n (Literal, Bool)
pLit    = P.pTokMaybe f
 where  f (KA (KLiteral l b)) = Just (l, b)
        f _                   = Nothing


-- | Parse a numeric literal, with source position.
pLitSP :: Parser n ((Literal, Bool), SourcePos)
pLitSP  = P.pTokMaybeSP f
 where  f (KA (KLiteral l b)) = Just (l, b)
        f _                   = Nothing


-- | Parse a variable.
pVar :: Parser n n
pVar    =   P.pTokMaybe f
        <?> "a variable"
 where  f (KN (KVar n))         = Just n
        f _                     = Nothing


-- | Parse a variable, with source position.
pVarSP :: Parser n (n, SourcePos)
pVarSP  =   P.pTokMaybeSP f
        <?> "a variable"
 where  f (KN (KVar n))         = Just n
        f _                     = Nothing


-- | Parse a variable of a specific name, with its source position.
pVarNamedSP :: String -> Parser String SourcePos
pVarNamedSP str 
        = fmap snd (P.pTokMaybeSP f <?> "a variable")
 where  f (KN (KVar n)) | n == str = Just n
        f _                        = Nothing


-- | Parse a variable-like name as a flat string.
pVarName   :: Pretty n => Parser n Text
pVarName  =  P.pTokMaybe f
         <?> "a name"
 where  f (KN (KVar n))         = Just $ (Text.pack $ renderPlain $ ppr n)
        f _                     = Nothing


-- | Parse a deBruijn index.
pIndex :: Parser n Int
pIndex  =   P.pTokMaybe f
        <?> "an index"
 where  f (KA (KIndex i))       = Just i
        f _                     = Nothing


-- | Parse a deBruijn index, with source position.
pIndexSP :: Parser n (Int, SourcePos)
pIndexSP  =   P.pTokMaybeSP f
        <?> "an index"
 where  f (KA (KIndex i))       = Just i
        f _                     = Nothing


-- | Parse an infix operator.
pOpSP    :: Parser n (String, SourcePos)
pOpSP    = P.pTokMaybeSP f
 where  f (KA (KOp str))  = Just str
        f _               = Nothing


-- | Parse an infix operator used as a variable.
pOpVarSP :: Parser n (String, SourcePos)
pOpVarSP = P.pTokMaybeSP f
 where  f (KA (KOpVar str))  = Just str
        f _                  = Nothing


-- | Parse a pragma.
pPragmaSP :: Parser n (Text, SourcePos)
pPragmaSP = P.pTokMaybeSP f
 where  f (KA (KPragma txt))  = Just txt
        f _                   = Nothing

-------------------------------------------------------------------------------
-- | Parse a `Symbol`.
pSym :: Symbol  -> Parser n SourcePos
pSym ss = P.pTokSP (KA (KSymbol ss))


-- | Parse a `Keyword`.
pKey :: Keyword -> Parser n SourcePos
pKey kw = P.pTokSP (KA (KKeyword kw))


-------------------------------------------------------------------------------
-- | Parse an atomic token.
pTok   :: TokenAtom -> Parser n ()
pTok k     = P.pTok (KA k)


-- | Parse an atomic token, yielding its source position.
pTokSP :: TokenAtom -> Parser n SourcePos
pTokSP k   = P.pTokSP (KA k)


-- | Parse an atomic token and return some value.
pTokAs :: TokenAtom -> a -> Parser n a
pTokAs k x = P.pTokAs (KA k) x


-- | Parse an atomic token and return source position and value.
pTokAsSP :: TokenAtom -> a -> Parser n (a, SourcePos)
pTokAsSP k x = P.pTokAsSP (KA k) x


