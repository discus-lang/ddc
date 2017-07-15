{-# LANGUAGE PatternSynonyms #-}
module DDC.Core.Lexer.Offside.Base
        ( Lexeme        (..)
        , pattern LexemeBraceBra
        , pattern LexemeBraceKet
        , pattern LexemeRoundBra
        , pattern LexemeRoundKet
        , pattern LexemeSemiColon
        , pattern LexemeOffsideClosingBrace
        , pattern LexemeLet
        , pattern LexemeIn

        , sourcePosOfLexeme
        , lexemesOfLocated
        , locatedOfLexemes
        , Context       (..)

        , isToken,      isKNToken,      isKeyword
        , dropNewLinesLexeme)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos


-- | Holds a real token or start symbol which is used to apply the offside rule.
data Lexeme n
        -- | Wrap a token from the source program.
        = LexemeToken           SourcePos (Token n)

        -- | Signal that the line has started at this position.
        | LexemeStartLine       SourcePos

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      SourcePos
        deriving (Eq, Show)


pattern LexemeBraceBra  sp              = LexemeToken sp (KA (KSymbol  SBraceBra))
pattern LexemeBraceKet  sp              = LexemeToken sp (KA (KSymbol  SBraceKet))
pattern LexemeRoundBra  sp              = LexemeToken sp (KA (KSymbol  SRoundBra))
pattern LexemeRoundKet  sp              = LexemeToken sp (KA (KSymbol  SRoundKet))
pattern LexemeSemiColon sp              = LexemeToken sp (KA (KSymbol  SSemiColon))
pattern LexemeOffsideClosingBrace sp    = LexemeToken sp (KM (KOffsideClosingBrace))
pattern LexemeLet       sp              = LexemeToken sp (KA (KKeyword ELet))
pattern LexemeIn        sp              = LexemeToken sp (KA (KKeyword EIn))


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


-- | Convert lexemes to located tokens,
--   discarding any StartLine or StartBlock indicators.
locatedOfLexemes :: [Lexeme n] -> [Located (Token n)]
locatedOfLexemes ls
 = [Located sp t | LexemeToken sp t <- ls ]


-- | What lexer context we're currently inside.
data Context
        -- | Explicit { context.
        = ContextExplicitBrace

        -- | Explicit ( context.
        | ContextExplicitParen

        -- | Implicitly inserted let-like context.
        | ContextExplicitLet

        -- | Implicitly inserted '{' context at the given level.
        | ContextImplicitBrace Int

        -- | Implicitly inserted '{' context after a let-keyword.
        | ContextImplicitLet   Int
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


-- | Drop newline tokens at the front of this stream.
dropNewLinesLexeme :: Eq n => [Lexeme n] -> [Lexeme n]
dropNewLinesLexeme ll
 = case ll of
        []      -> []

        LexemeToken _ (KM KNewLine) : ts
         -> dropNewLinesLexeme ts

        _       -> ll
