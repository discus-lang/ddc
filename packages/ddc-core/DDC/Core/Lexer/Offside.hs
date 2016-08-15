
-- | Apply the offside rule to a token stream to add braces.
module DDC.Core.Lexer.Offside
        ( Lexeme        (..)
        , applyOffside
        , addStarts)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos


---------------------------------------------------------------------------------------------------
-- | Holds a real token or start symbol which is used to apply the offside rule.
data Lexeme n
        = LexemeToken           (Located (Tok n))
        | LexemeStartLine       Int

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      Int
        deriving (Eq, Show)


-- | Parenthesis that we're currently inside. 
data Paren
        = ParenRound
        | ParenBrace
        deriving Show

-- | What column number the current layout context started in.
type Context
        = Int


-- | Apply the offside rule to this token stream.
--
--    It should have been processed with addStarts first to add the
--    LexemeStartLine/LexemeStartLine tokens.
--
--    Unlike the definition in the Haskell 98 report, we explicitly track
--    which parenthesis we're inside. We use these to partly implement
--    the layout rule that says we much check for entire parse errors to
--    perform the offside rule.
applyOffside 
        :: (Eq n, Show n)
        => [Paren]              -- ^ What parenthesis we're inside.
        -> [Context]            -- ^ Current layout context.
        -> [Lexeme n]           -- ^ Input lexemes.
        -> [Located (Tok n)]

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



-- addStarts --------------------------------------------------------------------------------------
-- | Add block and line start tokens to this stream.
--
--   This is identical to the definition in the Haskell98 report,
--   except that we also use multi-token starting strings like
--   'imports' 'foreign' 'type'.
addStarts :: (Eq n, Show n) => [Located (Tok n)] -> [Lexeme n]
addStarts ts
 = case dropNewLines ts of

        -- If the first lexeme of a module is not '{' then start a new block.
        (t1 : tsRest)
          |  not $ or $ map (isToken t1) [KA (KSymbol SBraceBra)]
          -> LexemeStartBlock (columnOfLocated t1) : addStarts' (t1 : tsRest)

          | otherwise
          -> addStarts' (t1 : tsRest)

        -- empty file
        []      -> []


addStarts'  :: Eq n => [Located (Tok n)] -> [Lexeme n]
addStarts' ts
        -- Block started at end of input.
        | Just (ts1, ts2)       <- splitBlockStart ts
        , []                    <- dropNewLines ts2
        = [LexemeToken t | t <- ts1] 
                ++ [LexemeStartBlock 0]

        -- Standard block start.
        --  If there is not an open brace after a block start sequence then
        --  insert a new one.
        | Just (ts1, ts2)       <- splitBlockStart ts
        , t2 : tsRest           <- dropNewLines ts2
        , not $ isToken t2 (KA (KSymbol SBraceBra))
        = [LexemeToken t | t <- ts1]
                ++ [LexemeStartBlock (columnOfLocated t2)]
                ++ addStarts' (t2 : tsRest)

        -- check for start of list
        | t1 : ts'              <- ts
        , isToken t1 (KA (KSymbol SBraceBra))
        = LexemeToken t1    : addStarts' ts'

        -- check for end of list
        | t1 : ts'              <- ts
        , isToken t1 (KA (KSymbol SBraceKet))
        = LexemeToken t1    : addStarts' ts'

        -- check for start of new line
        | t1 : ts'              <- ts
        , isToken t1 (KM KNewLine)
        , t2 : tsRest   <- dropNewLines ts'
        , not $ isToken t2 (KA (KSymbol SBraceBra))
        = LexemeStartLine (columnOfLocated t2) 
                : addStarts' (t2 : tsRest)

        -- eat up trailine newlines
        | t1 : ts'              <- ts
        , isToken t1 (KM KNewLine)
        = addStarts' ts'

        -- a regular token
        | t1 : ts'              <- ts
        = LexemeToken t1    : addStarts' ts'

        -- end of input
        | otherwise
        = []


-- | Drop newline tokens at the front of this stream.
dropNewLines :: Eq n => [Located (Tok n)] -> [Located (Tok n)]
dropNewLines []              = []
dropNewLines (t1:ts)
        | isToken t1 (KM KNewLine)
        = dropNewLines ts

        | otherwise
        = t1 : ts


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


-- | Check if a token is one that starts a block of statements.
splitBlockStart 
        :: [Located (Tok n)] 
        -> Maybe ([Located (Tok n)], [Located (Tok n)])

splitBlockStart toks

 -- export type
 |  t1@(Located _ (KA (KKeyword EExport)))
  : t2@(Located _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- export value
 |  t1@(Located _ (KA (KKeyword EExport)))
  : t2@(Located _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- export foreign X value
 |  t1@(Located _ (KA (KKeyword EExport)))
  : t2@(Located _ (KA (KKeyword EForeign)))
  : t3                                  
  : t4@(Located _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import type
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import value
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import data
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EData)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import foreign X type
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EForeign)))
  : t3                                  
  : t4@(Located _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import foreign X capability
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EForeign)))
  : t3
  : t4@(Located _ (KA (KKeyword ECapability)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import foreign X value
 |  t1@(Located _ (KA (KKeyword EImport)))
  : t2@(Located _ (KA (KKeyword EForeign)))
  : t3                                  
  : t4@(Located _ (KA (KKeyword EValue)))
  : ts    
 <- toks = Just ([t1, t2, t3, t4], ts)
 
 |  t1@(Located _ (KA (KKeyword EDo)))     : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword EOf)))     : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword ELetRec))) : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword EWhere)))  : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword EExport))) : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword EImport))) : ts    <- toks = Just ([t1], ts)
 |  t1@(Located _ (KA (KKeyword EMatch)))  : ts    <- toks = Just ([t1], ts)

 | otherwise                                             
 = Nothing


-- Utils ------------------------------------------------------------------------------------------
-- | Test whether this wrapper token matches.
isToken :: Eq n => Located (Tok n) -> Tok n -> Bool
isToken (Located _ tok) tok2 = tok == tok2


-- | Test whether this wrapper token matches.
isKNToken :: Eq n => Located (Tok n) -> Bool
isKNToken (Located _ (KN _))    = True
isKNToken _                     = False


-- | When generating new source tokens, take the position from the first
--   non-newline token in this list
newCBra      :: [Lexeme n] -> Located (Tok n)
newCBra ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SBraceBra))


newCKet      :: [Lexeme n] -> Located (Tok n)
newCKet ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SBraceKet))


newSemiColon :: [Lexeme n] -> Located (Tok n)
newSemiColon ts
 = case takeTok ts of
        Located sp _    -> Located sp (KA (KSymbol SSemiColon))


-- | This is injected by `applyOffside` when it finds an explit close
--   brace in a position where it would close a synthetic one.
newOffsideClosingBrace :: [Lexeme n] -> Located (Tok n)
newOffsideClosingBrace ts
 = case takeTok ts of
        Located sp _    -> Located sp (KM KOffsideClosingBrace)


takeTok :: [Lexeme n] -> Located (Tok n)
takeTok []      
 = Located (SourcePos "" 0 0) (KErrorJunk "") 

takeTok (l : ls)
 = case l of
        LexemeToken (Located _ (KM KNewLine))
         -> takeTok ls

        LexemeToken t           -> t
        LexemeStartLine  _      -> takeTok ls
        LexemeStartBlock _      -> takeTok ls

