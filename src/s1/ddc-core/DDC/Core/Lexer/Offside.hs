
-- | Apply the offside rule to a token stream to add braces.
module DDC.Core.Lexer.Offside
        ( Lexeme        (..)
        , applyOffside
        , addStarts)
where
import DDC.Core.Lexer.Offside.Starts
import DDC.Core.Lexer.Offside.Base
import DDC.Core.Lexer.Tokens


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
        -> [Located (Token n)]

-- Wait for the module header before we start applying the real offside rule.
-- This allows us to write 'module Name with letrec' all on the same line.
applyOffside [] (LexemeToken sp t : ts)
 |  isKeyword t EModule
 || isKNToken t
 = Located sp t : applyOffside [] ts

-- Enter into a top-level block in the module, and start applying the
-- offside rule within it.
-- The blocks are introduced by:
--      'exports' 'imports' 'letrec' 'where'
--      'import foreign MODE type'
--      'import foreign MODE capability'
--      'import foreign MODE value'
applyOffside [] ls
 | LexemeToken sp1 t1
         : LexemeStartBlock _spn n : ls' <- ls
 ,   isKeyword t1 EExport || isKeyword t1 EImport
  || isKeyword t1 ELetRec || isKeyword t1 EWhere
 = Located sp1 t1
        : newCBra ls'
        : applyOffside (ContextBraceImplicit n : []) ls'

 -- (import | export) (type | value) { ... }
 | LexemeToken sp1 t1 : LexemeToken sp2 t2
        : LexemeStartBlock _spn n : ls' <- ls
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EType     || isKeyword t2 EValue
 = Located sp1 t1     : Located sp2 t2
        : newCBra ls'
        : applyOffside (ContextBraceImplicit n : []) ls'

 -- (import | export) foreign X (type | capability | value) { ... }
 | LexemeToken sp1 t1 : LexemeToken sp2 t2 : LexemeToken sp3 t3 : LexemeToken sp4 t4
         : LexemeStartBlock _spn n : ls' <- ls
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EForeign
 , isKeyword t4 EType     || isKeyword t4 ECapability  || isKeyword t4 EValue
 = Located sp1 t1     : Located sp2 t2     : Located sp3 t3     : Located sp4 t4
        : newCBra ls'
        : applyOffside (ContextBraceImplicit n : []) ls'

-- At top level without a context.
-- Skip over everything until we get the 'with' in 'module Name with ...''
applyOffside [] (LexemeStartLine _ _  : ts)
 = applyOffside [] ts

applyOffside [] (LexemeStartBlock _ _ : ts)
 = applyOffside [] ts

-- line start
applyOffside cc@(ContextBraceImplicit m : cs) (t@(LexemeStartLine _ n) : ts)
 -- Add semicolon to get to the next statement in this block.
 | m == n        = newSemiColon ts : applyOffside cc ts

 -- End an implicit block.
 --  We need to keep the LexemeStartLine because this newline might
 --  be ending multiple blocks at once.
 | n <= m        = newCKet ts : applyOffside cs (t : ts)

 -- Indented continuation of this block.
 | otherwise     = applyOffside cc ts

-- We're not inside a context which would be closed by a newline.
applyOffside cc ((LexemeStartLine _sp _n) : ts)
 = applyOffside cc ts

-- block start
applyOffside cc (LexemeStartBlock _sp n : ts)
 = newCBra ts : applyOffside (ContextBraceImplicit n : cc) ts

-- push context for explicit open brace
applyOffside cc (LexemeToken sp t@(KA (KSymbol SBraceBra)) : ts)
 = Located sp t : applyOffside (ContextBraceExplicit : cc) ts

-- pop context for explicit close brace
applyOffside cc (LexemeToken sp t@(KA (KSymbol SBraceKet)) : ts)
 -- close brace matches an explicit open brace.
 | ContextBraceExplicit : cs    <- cc
 = Located sp t : applyOffside cs ts

 -- close brace where we had an implicit open brace.
 | _tNext : _     <- dropNewLinesLexeme ts
 = [newOffsideClosingBrace ts]

-- push context for explict open paren.
applyOffside cc     (LexemeToken sp t@(KA (KSymbol SRoundBra))  : ts)
 = Located sp t : applyOffside (ContextParenExplicit : cc) ts

applyOffside cc (lt@(LexemeToken sp  t@(KA (KSymbol SRoundKet))) : ts)
 -- force close of block on close paren.
 -- This partially handles the crazy (Note 5) rule from the Haskell98 standard.
 | ContextBraceImplicit _ : cs <- cc
 = newCKet ts : applyOffside cs (lt : ts)

 -- pop context for explicit close paren.
 | ContextParenExplicit : cs <- cc
 = Located sp t : applyOffside cs ts

-- pass over tokens.
applyOffside cc (LexemeToken sp' t : ts)
 = Located sp' t : applyOffside cc ts

applyOffside [] []
 = []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside (_ : cs) []
 = newCKet [] : applyOffside cs []

