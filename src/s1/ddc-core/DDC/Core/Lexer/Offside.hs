
-- | Apply the offside rule to a token stream to add braces.
module DDC.Core.Lexer.Offside
        ( Lexeme        (..)
        , applyOffside
        , addStarts
        , locatedOfLexemes
        , lexemesOfLocated)
where
import DDC.Core.Lexer.Offside.Starts
import DDC.Core.Lexer.Offside.Base
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos


-- | Apply the offside rule to this token stream.
--
--    It should have been processed with addStarts first to add the
--    LexemeStartLine/LexemeStartLine tokens.
--
--    Unlike the definition in the Haskell 98 report, we explicitly track
--    which parenthesis we're inside. We use these to partly implement
--    the layout rule that says we much check for entire parse errors to
--    perform the offside rule.
--
applyOffside
        :: (Eq n, Show n)
        => [Context]            -- ^ Current layout context.
        -> [Lexeme n]           -- ^ Input lexemes.
        -> [Lexeme n]

-- Wait for the module header before we start applying the real offside rule.
-- This allows us to write 'module Name with letrec' all on the same line.
applyOffside [] (lt@(LexemeToken _sp t) : lts)
 |  isKeyword t EModule
 || isKNToken t
 = lt : applyOffside [] lts

-- Enter into a top-level block in the module, and start applying the
-- offside rule within it.
-- The blocks are introduced by:
--      'exports' 'imports' 'letrec' 'where'
--      'import foreign MODE type'
--      'import foreign MODE capability'
--      'import foreign MODE value'
applyOffside [] lts
 | lt1@(LexemeToken _sp1 t1)
         : LexemeStartBlock spn : lts' <- lts
 ,   isKeyword t1 EExport || isKeyword t1 EImport
  || isKeyword t1 ELetRec || isKeyword t1 ELet || isKeyword t1 EWhere
 = lt1  : LexemeBraceBra spn
        : applyOffside (ContextImplicitBrace (sourcePosColumn spn) : []) lts'

 -- (import | export) (type | value) { ... }
 | lt1@(LexemeToken _sp1 t1)
        : lt2@(LexemeToken _sp2 t2)
        : LexemeStartBlock spn : lts' <- lts
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EType     || isKeyword t2 EValue
 = lt1  : lt2
        : LexemeBraceBra spn
        : applyOffside (ContextImplicitBrace (sourcePosColumn spn) : []) lts'

 -- (import | export) foreign X (type | capability | value) { ... }
 | lt1@(LexemeToken _sp1 t1)
        : lt2@(LexemeToken _sp2  t2)
        : lt3@(LexemeToken _sp3 _t3)
        : lt4@(LexemeToken _sp4  t4)
        : LexemeStartBlock spn : lts' <- lts
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EForeign
 , isKeyword t4 EType     || isKeyword t4 ECapability  || isKeyword t4 EValue
 = lt1  : lt2 : lt3 : lt4
        : LexemeBraceBra spn
        : applyOffside (ContextImplicitBrace (sourcePosColumn spn) : []) lts'


-- Top-level ----------------------------------------------
-- At top level without a context.
-- Skip over everything until we get the 'with' in 'module Name with ...''
applyOffside [] (LexemeStartLine _  : lts)
 = applyOffside [] lts

applyOffside [] (LexemeStartBlock _ : lts)
 = applyOffside [] lts


-- Private / Extend ---------------------------------------
-- explicit let-context open
applyOffside cc (lt1@(LexemeToken _sp1 _t1) : lts1)
 |   isToken lt1 (KA (KKeyword EPrivate))
  || isToken lt1 (KA (KKeyword EExtend))
 = lt1 : applyOffside (ContextExplicitLet : cc) lts1

-- explicit let-context close
applyOffside cc (lt1@(LexemeToken _sp1 _t1) : lts1)
 | ContextExplicitLet : cs <- cc
 , isToken lt1 (KA (KKeyword EIn))
 = lt1 : applyOffside cs lts1


-- Let ----------------------------------------------------
-- let-context start.
applyOffside cc (lt1@(LexemeToken sp1 _t1) : lts1)
 |   isToken lt1 (KA (KKeyword ELet))
  || isToken lt1 (KA (KKeyword ERec))
  || isToken lt1 (KA (KKeyword ELetRec))
  || isToken lt1 (KA (KKeyword ELetCase))
 , lt2 : lts2   <- dropNewLinesLexeme lts1
 = if isToken lt2 (KA (KSymbol SBraceBra))
    -- Explicit let-context.
    then lt1 : applyOffside (ContextExplicitLet : cc) lts1

    -- Implicit let-context.
    else let  col   = sourcePosColumn $ sourcePosOfLexeme lt2
         in   lt1   : LexemeBraceBra sp1 : lt2
                    : applyOffside (ContextImplicitLet col : cc) lts2

-- let-context newline.
applyOffside cc@(ContextImplicitLet m : cs) (lt1@(LexemeStartLine sp) : lts1)
 | m == sourcePosColumn sp
 = LexemeSemiColon sp : applyOffside cc lts1

 --  We need to keep the LexemeStartLine because this newline might
 --  be ending multiple blocks at once.
 | m >= sourcePosColumn sp
 , lt2 : _lts2   <- dropNewLinesLexeme lts1
 , not $ isToken lt2 (KA (KKeyword EIn))
 = LexemeBraceKet sp : applyOffside cs (lt1 : lts1)

 | otherwise
 = applyOffside cc lts1

-- let-context close.
applyOffside    (ContextImplicitLet _m : cs) (lt1@(LexemeIn sp) : lts)
 = LexemeBraceKet sp : lt1 : applyOffside cs lts


-- Brace --------------------------------------------------
-- line start
applyOffside cc@(ContextImplicitBrace m : cs) (lt@(LexemeStartLine sp) : lts)
 -- Add semicolon to get to the next statement in this block.
 | m == sourcePosColumn sp
 = LexemeSemiColon sp   : applyOffside cc lts

 -- End an implicit block.
 --  We need to keep the LexemeStartLine because this newline might
 --  be ending multiple blocks at once.
 | m >= sourcePosColumn sp
 = LexemeBraceKet sp    : applyOffside cs (lt : lts)

 -- Indented continuation of this block.
 | otherwise
 = applyOffside cc lts

-- We're not inside a context which would be closed by a newline.
applyOffside cc (LexemeStartLine _sp : lts)
 = applyOffside cc lts

-- standard block start
applyOffside cc (LexemeStartBlock sp : lts)
 = LexemeBraceBra sp
        : applyOffside (ContextImplicitBrace (sourcePosColumn sp)  : cc) lts

-- push context for explicit open brace
applyOffside cc (lt@(LexemeBraceBra _sp) : lts)
 = lt   : applyOffside (ContextExplicitBrace : cc) lts

-- pop context for explicit close brace
applyOffside cc (lt@(LexemeBraceKet  sp) : lts)
 -- close brace matches an explicit open brace.
 | ContextExplicitBrace : cs    <- cc
 = lt   : applyOffside cs lts

 -- close brace where we had an implicit open brace.
 | _tNext : _     <- dropNewLinesLexeme lts
 = LexemeOffsideClosingBrace sp : lts


-- Paren --------------------------------------------------
-- push context for explict open paren.
applyOffside cc (lt@(LexemeRoundBra _sp) : lts)
 = lt   : applyOffside (ContextExplicitParen : cc) lts

applyOffside cc (lt@(LexemeRoundKet  sp) : lts)
 -- force close of block on close paren.
 -- This partially handles the crazy (Note 5) rule from the Haskell98 standard.
 | ContextImplicitBrace _ : cs <- cc
 = LexemeBraceKet sp    : applyOffside cs (lt : lts)

 -- pop context for explicit close paren.
 | ContextExplicitParen : cs <- cc
 = lt : applyOffside cs lts


-- Generic ------------------------------------------------
-- pass over tokens.
applyOffside cc (lt : lts)
 = lt : applyOffside cc lts

applyOffside [] []
 = []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside (ContextImplicitBrace _ : cs) []
 = LexemeBraceKet (SourcePos "" 0 0) : applyOffside cs []

applyOffside (ContextImplicitLet _   : cs) []
 = LexemeBraceKet (SourcePos "" 0 0) : applyOffside cs []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside (_ : cs) []
 = applyOffside cs []

