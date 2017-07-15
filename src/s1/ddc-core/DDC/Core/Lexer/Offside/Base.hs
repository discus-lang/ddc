{-# LANGUAGE PatternSynonyms #-}
module DDC.Core.Lexer.Offside.Base
        ( Lexeme        (..)
        , sourcePosOfLexeme
        , lexemesOfLocated

        , Context       (..)

        , isToken,      isKNToken,      isKeyword
        , newOffsideClosingBrace
        , takeTok
        , dropNewLinesLexeme

        , pattern LocatedBraceBra
        , pattern LocatedBraceKet
        , pattern LocatedSemiColon
        , pattern LocatedOffsideClosingBrace)
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


-- | Test whether this token is the given keyword.
isKeyword tok k
 = case tok of
        KA (KKeyword k')        -> k == k'
        _                       -> False


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


pattern LocatedBraceBra  sp             = Located sp (KA (KSymbol SBraceBra))
pattern LocatedBraceKet  sp             = Located sp (KA (KSymbol SBraceKet))
pattern LocatedSemiColon sp             = Located sp (KA (KSymbol SSemiColon))
pattern LocatedOffsideClosingBrace sp   = Located sp (KM (KOffsideClosingBrace))

