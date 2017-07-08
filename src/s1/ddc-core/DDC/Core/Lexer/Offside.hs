
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
--    TODO: The [Paren] and [Context] args are running in lockstep, combine
--    this info into the same structure. Track let.. in as a separate sort of
--    parenthesis. If we know we're inside a let context then we'll be able
--    to work out when to insert ';' on newlines.
--
applyOffside
        :: (Eq n, Show n)
        => [Paren]              -- ^ What parenthesis we're inside.
        -> [Context]            -- ^ Current layout context.
        -> [Lexeme n]           -- ^ Input lexemes.
        -> [Located (Token n)]

-- Wait for the module header before we start applying the real offside rule.
-- This allows us to write 'module Name with letrec' all on the same line.
applyOffside ps [] (LexemeToken t : ts)
        |   isKeyword t EModule
         || isKNToken t
        = t : applyOffside ps [] ts


-- Enter into a top-level block in the module, and start applying the
-- offside rule within it.
-- The blocks are introduced by:
--      'exports' 'imports' 'letrec' 'where'
--      'import foreign MODE type'
--      'import foreign MODE capability'
--      'import foreign MODE value'
applyOffside ps [] ls
        | LexemeToken t1
                : (LexemeStartBlock n) : ls' <- ls
        ,   isKeyword t1 EExport
         || isKeyword t1 EImport
         || isKeyword t1 ELetRec
         || isKeyword t1 EWhere
        = t1 : newCBra ls'      : applyOffside (ParenBrace : ps) [n] ls'

        -- (import | export) (type | value) { ... }
        | LexemeToken t1 : LexemeToken t2
                : LexemeStartBlock n : ls' <- ls
        , isKeyword t1 EImport   || isKeyword t1 EExport
        , isKeyword t2 EType     || isKeyword t2 EValue
        = t1 : t2 : newCBra ls' : applyOffside (ParenBrace : ps) [n] ls'

        -- (import | export) foreign X (type | capability | value) { ... }
        | LexemeToken t1 : LexemeToken t2 : LexemeToken t3 : LexemeToken t4
                : LexemeStartBlock n : ls' <- ls
        , isKeyword t1 EImport   || isKeyword t1 EExport
        , isKeyword t2 EForeign
        , isKeyword t4 EType     || isKeyword t4 ECapability  || isKeyword t4 EValue
        = t1 : t2 : t3 : t4 : newCBra ls' : applyOffside (ParenBrace : ps) [n] ls'

-- At top level without a context.
-- Skip over everything until we get the 'with' in 'module Name with ...''
applyOffside ps [] (LexemeStartLine _  : ts)
        = applyOffside ps [] ts

applyOffside ps [] (LexemeStartBlock _ : ts)
        = applyOffside ps [] ts


-- line start
applyOffside ps mm@(m : ms) (t@(LexemeStartLine n) : ts)
        -- add semicolon to get to the next statement in this block
        | m == n
        = newSemiColon ts : applyOffside ps mm ts

        -- end a block
        | n <= m
        = case ps of
                -- Closed a block that we're inside, ok.
                ParenBrace : ps'
                  -> newCKet ts : applyOffside ps' ms (t : ts)

                -- We're supposed to close the block we're inside, but we're
                -- still inside an open '(' context. Just keep passing the
                -- tokens through, and let the parser give its error when
                -- it gets to it.
                ParenRound : _
                  -> applyOffside ps ms ts

                -- We always push an element of the layout context
                -- at the same time as a paren context, so this shouldn't happen.
                _ -> error $ "ddc-core: paren / layout context mismatch."

        -- indented continuation of this statement
        | otherwise
        = applyOffside ps mm ts


-- block start
applyOffside ps mm@(m : ms) (LexemeStartBlock n : ts)
        -- enter into a nested context
        | n > m
        = newCBra ts : applyOffside (ParenBrace : ps) (n : m : ms) ts

        -- new context starts less than the current one.
        --  This should never happen,
        --    provided addStarts works.
        | tNext : _    <- dropNewLinesLexeme ts
        = error $ "ddc-core: layout error on " ++ show tNext ++ "."

        -- new context cannot be less indented than outer one
        --  This should never happen,
        --   as there is no lexeme to start a new context at the end of the file.
        | []            <- dropNewLinesLexeme ts
        = error $ "ddc-core: tried to start new context at end of file."

        -- an empty block
        | otherwise
        = newCBra ts : newCKet ts : applyOffside ps mm (LexemeStartLine n : ts)


-- push context for explicit open brace
applyOffside ps ms
        (LexemeToken t@(Located _ (KA (KSymbol SBraceBra))) : ts)
        = t : applyOffside (ParenBrace : ps) (0 : ms) ts

-- pop context from explicit close brace
applyOffside ps mm
        (LexemeToken t@(Located _ (KA (KSymbol SBraceKet))) : ts)

        -- make sure that explict open braces match explicit close braces
        | 0 : ms                <- mm
        , ParenBrace : ps'      <- ps
        = t : applyOffside ps' ms ts

        -- nup
        | _tNext : _     <- dropNewLinesLexeme ts
        = [newOffsideClosingBrace ts]


-- push context for explict open paren.
applyOffside ps ms
        (    LexemeToken t@(Located _ (KA (KSymbol SRoundBra))) : ts)
        = t : applyOffside (ParenRound : ps) ms ts

-- force close of block on close paren.
-- This partially handles the crazy (Note 5) rule from the Haskell98 standard.
applyOffside (ParenBrace : ps) (m : ms)
        (lt@(LexemeToken   (Located _ (KA (KSymbol SRoundKet)))) : ts)
 | m /= 0
 = newCKet ts : applyOffside ps ms (lt : ts)

-- pop context for explicit close paren.
applyOffside (ParenRound : ps) ms
        (    LexemeToken t@(Located _ (KA (KSymbol SRoundKet))) : ts)
        = t : applyOffside ps ms ts

-- pass over tokens.
applyOffside ps ms (LexemeToken t : ts)
        = t : applyOffside ps ms ts

applyOffside _ [] []        = []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside ps (_ : ms) []
        = newCKet [] : applyOffside ps ms []


isKeyword (Located _ tok) k
 = case tok of
        KA (KKeyword k')        -> k == k'
        _                       -> False

