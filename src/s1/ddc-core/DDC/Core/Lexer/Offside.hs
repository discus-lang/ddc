
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
applyOffside [] (LexemeToken t : ts)
 |   isKeyword t EModule
 || isKNToken t
 = t : applyOffside [] ts

-- Enter into a top-level block in the module, and start applying the
-- offside rule within it.
-- The blocks are introduced by:
--      'exports' 'imports' 'letrec' 'where'
--      'import foreign MODE type'
--      'import foreign MODE capability'
--      'import foreign MODE value'
applyOffside [] ls
 | LexemeToken t1
         : (LexemeStartBlock n) : ls' <- ls
 ,   isKeyword t1 EExport
  || isKeyword t1 EImport
  || isKeyword t1 ELetRec
  || isKeyword t1 EWhere
 = t1 : newCBra ls'      : applyOffside (ContextBraceImplicit n : []) ls'

 -- (import | export) (type | value) { ... }
 | LexemeToken t1 : LexemeToken t2
         : LexemeStartBlock n : ls' <- ls
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EType     || isKeyword t2 EValue
 = t1 : t2 : newCBra ls' : applyOffside (ContextBraceImplicit n : []) ls'

 -- (import | export) foreign X (type | capability | value) { ... }
 | LexemeToken t1 : LexemeToken t2 : LexemeToken t3 : LexemeToken t4
         : LexemeStartBlock n : ls' <- ls
 , isKeyword t1 EImport   || isKeyword t1 EExport
 , isKeyword t2 EForeign
 , isKeyword t4 EType     || isKeyword t4 ECapability  || isKeyword t4 EValue
 = t1 : t2 : t3 : t4 : newCBra ls' : applyOffside (ContextBraceImplicit n : []) ls'

-- At top level without a context.
-- Skip over everything until we get the 'with' in 'module Name with ...''
applyOffside [] (LexemeStartLine _  : ts)
 = applyOffside [] ts

applyOffside [] (LexemeStartBlock _ : ts)
 = applyOffside [] ts

-- line start
applyOffside cc@(ContextBraceImplicit m : cs) (t@(LexemeStartLine n) : ts)
 -- Add semicolon to get to the next statement in this block.
 | m == n        = newSemiColon ts : applyOffside cc ts

 -- End an implicit block.
 --  We need to keep the LexemeStartLine because this newline might
 --  be ending multiple blocks at once.
 | n <= m        = newCKet ts : applyOffside cs (t : ts)

 -- Indented continuation of this block.
 | otherwise     = applyOffside cc ts

-- We're not inside a context which would be closed by a newline.
applyOffside cc ((LexemeStartLine _n) : ts)
 = applyOffside cc ts

-- block start
applyOffside cc (LexemeStartBlock n : ts)
 = newCBra ts : applyOffside (ContextBraceImplicit n : cc) ts

-- push context for explicit open brace
applyOffside cc (LexemeToken t@(Located _ (KA (KSymbol SBraceBra))) : ts)
 = t : applyOffside (ContextBraceExplicit : cc) ts

-- pop context for explicit close brace
applyOffside cc (LexemeToken t@(Located _ (KA (KSymbol SBraceKet))) : ts)
 -- close brace matches an explicit open brace.
 | ContextBraceExplicit : cs    <- cc
 = t : applyOffside cs ts

 -- close brace where we had an implicit open brace.
 | _tNext : _     <- dropNewLinesLexeme ts
 = [newOffsideClosingBrace ts]

-- push context for explict open paren.
applyOffside cc (LexemeToken     t@(Located _ (KA (KSymbol SRoundBra))) : ts)
 = t : applyOffside (ContextParenExplicit : cc) ts

applyOffside cc (lt@(LexemeToken t@(Located _ (KA (KSymbol SRoundKet)))) : ts)
 -- force close of block on close paren.
 -- This partially handles the crazy (Note 5) rule from the Haskell98 standard.
 | ContextBraceImplicit _ : cs <- cc
 = newCKet ts : applyOffside cs (lt : ts)

 -- pop context for explicit close paren.
 | ContextParenExplicit : cs <- cc
 = t : applyOffside cs ts

-- pass over tokens.
applyOffside cc (LexemeToken t : ts)
 = t : applyOffside cc ts

applyOffside [] []
 = []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside (_ : cs) []
 = newCKet [] : applyOffside cs []

