{-# LANGUAGE PatternSynonyms #-}
module DDC.Core.Lexer.Offside.Base
        ( Lexeme        (..)
        , sourcePosOfLexeme
        , lexemesOfLocated

        , Context       (..)

        , isToken,      isKNToken,      isKeyword
        , newCBra,      newCKet
        , newSemiColon
        , newOffsideClosingBrace
        , takeTok
        , dropNewLinesLexeme)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos


-- | Holds a real token or start symbol which is used to apply the offside rule.
data Lexeme n
        = LexemeToken           SourcePos (Token n)
        | LexemeStartLine       SourcePos

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      SourcePos
        deriving (Eq, Show)


-- | Take the source position of a Lexeme.
sourcePosOfLexeme :: Lexeme n -> SourcePos
sourcePosOfLexeme ll
 = case ll of
        LexemeToken sp _        -> sp
        LexemeStartLine  sp     -> sp
        LexemeStartBlock sp     -> sp


-- | Convert located tokens into our lexeme type.
lexemesOfLocated :: [Located (Token n)] -> [Lexeme n]
lexemesOfLocated lts
 = map (\(Located sp t) -> LexemeToken sp t) lts




-- | What lexer context we're currently inside.
data Context
        -- | Explicit { context.
        = ContextBraceExplicit

        -- | Explicit ( context.
        | ContextParenExplicit

        -- | Implicitly inserted '{' context at the given level.
        | ContextBraceImplicit Int
        deriving Show


-- | Test whether this wrapper token matches.
isToken :: Eq n => Lexeme n -> Token n -> Bool
isToken (LexemeToken _ tok) tok2
                = tok == tok2
isToken _ _     = False


-- | Test whether this wrapper token matches.
isKNToken :: Eq n => Token n -> Bool
isKNToken (KN _)                = True
isKNToken _                     = False


isKeyword tok k
 = case tok of
        KA (KKeyword k')        -> k == k'
        _                       -> False


-- | When generating new source tokens, take the position from the first
--   non-newline token in this list
newCBra      :: [Lexeme n] -> Located (Token n)
newCBra ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SBraceBra))


newCKet      :: [Lexeme n] -> Located (Token n)
newCKet ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SBraceKet))


newSemiColon :: [Lexeme n] -> Located (Token n)
newSemiColon ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SSemiColon))


-- | This is injected by `applyOffside` when it finds an explit close
--   brace in a position where it would close a synthetic one.
newOffsideClosingBrace :: [Lexeme n] -> Located (Token n)
newOffsideClosingBrace ts
 = case takeTok ts of
        Located sp _    -> Located sp (KM KOffsideClosingBrace)


takeTok :: [Lexeme n] -> Located (Token n)
takeTok []
 = Located (SourcePos "" 0 0) (KErrorJunk "")

takeTok (l : ls)
 = case l of
        LexemeToken _ (KM KNewLine)     -> takeTok ls
        LexemeToken loc t               -> Located loc t
        LexemeStartLine  _              -> takeTok ls
        LexemeStartBlock _              -> takeTok ls


-- | Drop newline tokens at the front of this stream.
dropNewLinesLexeme :: Eq n => [Lexeme n] -> [Lexeme n]
dropNewLinesLexeme ll
 = case ll of
        []      -> []

        LexemeToken _ (KM KNewLine) : ts
         -> dropNewLinesLexeme ts

        _       -> ll

