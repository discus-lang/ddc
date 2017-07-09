
module DDC.Core.Lexer.Offside.Base
        ( Lexeme        (..)
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
        = LexemeToken           (Located (Token n))
        | LexemeStartLine       Int

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      Int
        deriving (Eq, Show)


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
isToken :: Eq n => Located (Token n) -> Token n -> Bool
isToken (Located _ tok) tok2
        = tok == tok2


-- | Test whether this wrapper token matches.
isKNToken :: Eq n => Located (Token n) -> Bool
isKNToken (Located _ (KN _))    = True
isKNToken _                     = False


isKeyword (Located _ tok) k
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
        LexemeToken (Located _ (KM KNewLine))
         -> takeTok ls

        LexemeToken t           -> t
        LexemeStartLine  _      -> takeTok ls
        LexemeStartBlock _      -> takeTok ls


-- | Drop newline tokens at the front of this stream.
dropNewLinesLexeme :: Eq n => [Lexeme n] -> [Lexeme n]
dropNewLinesLexeme ll
 = case ll of
        []                      -> []
        LexemeToken t1 : ts
         |  isToken t1 (KM KNewLine)
         -> dropNewLinesLexeme ts

        l : ls
         -> l : dropNewLinesLexeme ls


