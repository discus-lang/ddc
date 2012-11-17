
-- | Apply the offside rule to a token stream to add braces.
module DDC.Core.Lexer.Offside
        ( Lexeme        (..)
        , applyOffside
        , addStarts)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos
import DDC.Data.Token


-- | Holds a real token or start symbol which is used to apply the offside rule.
data Lexeme n
        = LexemeToken           (Token (Tok n))
        | LexemeStartLine       Int

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      Int
        deriving (Eq, Show)

type Context
        = Int

-- | Apply the offside rule to this token stream.
--   It should have been processed with addStarts first to add the
--   LexemeStartLine/LexemeStartLine tokens.
--
applyOffside 
        :: (Eq n, Show n) 
        => [Context] 
        -> [Lexeme n] 
        -> [Token (Tok n)]

-- Wait for the module header before we start applying the real offside rule. 
-- This allows us to write 'module Name with letrec' all on the same line.
applyOffside [] (LexemeToken t : ts) 
        |   isToken t (KA KModule)
         || isKNToken t
        = t : applyOffside [] ts

-- When we see the top-level letrec then enter into the outer-most context.
applyOffside [] (LexemeToken t1 : (LexemeStartBlock n) : ls)
        | isToken t1 (KA KLetRec)
        = t1 : newCBra ls : applyOffside [n] ls 

-- At top level without a context.
-- Skip over everything until we get the 'with' in 'module Name with ...''
applyOffside [] (LexemeStartLine _  : ts)
        = applyOffside [] ts 

applyOffside [] (LexemeStartBlock _ : ts)
        = applyOffside [] ts


-- line start
applyOffside mm@(m : ms) (t@(LexemeStartLine n) : ts)
        -- add semicolon to get to the next statement in this block
        | m == n
        = newSemiColon ts : applyOffside mm ts

        -- end a block
        -- we keep the StartLine token in the recursion in case we're ending
        -- multiple blocks difference from Haskell98: add a semicolon as well
        | n < m 
        = newSemiColon ts : newCKet ts : applyOffside ms (t : ts)

        -- indented continuation of this statement
        | otherwise
        = applyOffside mm ts


-- block start
applyOffside mm@(m : ms) (LexemeStartBlock n : ts)
        -- enter into a nested context
        | n > m
        = newCBra ts : applyOffside (n : m : ms) ts 

        -- new context starts less than the current one.
        --   This should never happen, 
        --     provided addStarts works.
        | tNext : _    <- dropNewLinesLexeme ts
        = error $ "DDC.Core.Lexer.Tokens.Offside: layout error on " ++ show tNext ++ "."

        -- new context cannot be less indented than outer one
        --   This should never happen,
        --      as there is no lexeme to start a new context at the end of the file
        | []            <- dropNewLinesLexeme ts
        = error "DDC.Core.Lexer.Tokens.Offside: tried to start new context at end of file."

        -- an empty block
        | otherwise
        = newCBra ts : newCKet ts : applyOffside mm (LexemeStartLine n : ts)


-- pop contexts from explicit close braces
applyOffside mm (LexemeToken t@Token { tokenTok = KA KBraceKet } : ts) 

        -- make sure that explict open braces match explicit close braces
        | 0 : ms        <- mm
        = t : applyOffside ms ts

        -- nup
        | _tNext : _     <- dropNewLinesLexeme ts
        = error "DDC.Core.Lexer.Tokens.Offside: no brace match"

        -- ISSUE #289: Better error message for non-matching braces.
        --      Can we trigger the above error in a test?
        --      If so we need a better message for it.


-- push contexts for explicit open braces
applyOffside ms (LexemeToken t@Token { tokenTok = KA KBraceBra } : ts)
        = t : applyOffside (0 : ms) ts

applyOffside ms (LexemeToken t : ts) 
        = t : applyOffside ms ts

applyOffside [] []          = []

-- close off remaining contexts once we've reached the end of the stream.
applyOffside (_ : ms) []    = newCKet [] : applyOffside ms []


-- addStarts ------------------------------------------------------------------
-- | Add block and line start tokens to this stream.
--      This is lifted straight from the Haskell98 report.
addStarts :: Eq n => [Token (Tok n)] -> [Lexeme n]
addStarts ts
 = case dropNewLines ts of

        -- If the first lexeme of a module is not '{' then start a new block.
        (t1 : tsRest)
          |  not $ or $ map (isToken t1) [KA KBraceBra]
          -> LexemeStartBlock (tokenColumn t1) : addStarts' (t1 : tsRest)

          | otherwise
          -> addStarts' (t1 : tsRest)

        -- empty file
        []      -> []


addStarts'  :: Eq n => [Token (Tok n)] -> [Lexeme n]
addStarts' []           = []
addStarts' (t1 : ts) 

        -- We're starting a block
        | isBlockStart t1
        , []            <- dropNewLines ts
        = LexemeToken t1    : [LexemeStartBlock 0]

        | isBlockStart t1
        , t2 : tsRest   <- dropNewLines ts
        , not $ isToken t2 (KA KBraceBra)
        = LexemeToken t1    : LexemeStartBlock (tokenColumn t2)
                            : addStarts' (t2 : tsRest)

        -- check for start of list
        | isToken t1 (KA KBraceBra)
        = LexemeToken t1    : addStarts' ts

        -- check for end of list
        | isToken t1 (KA KBraceKet)
        = LexemeToken t1    : addStarts' ts

        -- check for start of new line
        | isToken t1 (KM KNewLine)
        , t2 : tsRest   <- dropNewLines ts
        , not $ isToken t2 (KA KBraceBra)
        = LexemeStartLine (tokenColumn t2) 
                : addStarts' (t2 : tsRest)

        -- eat up trailine newlines
        | isToken t1 (KM KNewLine)
        = addStarts' ts

        -- a regular token
        | otherwise
        = LexemeToken t1    : addStarts' ts


-- | Drop newline tokens at the front fo this stream.
dropNewLines :: Eq n => [Token (Tok n)] -> [Token (Tok n)]
dropNewLines []              = []
dropNewLines (t1:ts)
        | isToken t1 (KM KNewLine)
        = dropNewLines ts

        | otherwise
        = t1 : ts


-- | Drop newline tokens at the front fo this stream.
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
isBlockStart :: Token (Tok n) -> Bool
isBlockStart Token { tokenTok = tok }
 = case tok of
        KA KDo          -> True
        KA KOf          -> True
        KA KLetRec      -> True
        KA KWhere       -> True
        _               -> False


-- Utils ----------------------------------------------------------------------
-- | Test whether this wrapper token matches.
isToken :: Eq n => Token (Tok n) -> Tok n -> Bool
isToken (Token tok _) tok2 = tok == tok2


-- | Test whether this wrapper token matches.
isKNToken :: Eq n => Token (Tok n) -> Bool
isKNToken (Token (KN _) _)      = True
isKNToken _                     = False


-- | When generating new source tokens, take the position from the first
--   non-newline token in this list
newCBra :: [Lexeme n] -> Token (Tok n)
newCBra ts      = (takeTok ts) { tokenTok = KA KBraceBra }


newCKet :: [Lexeme n] -> Token (Tok n)
newCKet ts      = (takeTok ts) { tokenTok = KA KBraceKet }


newSemiColon :: [Lexeme n] -> Token (Tok n)
newSemiColon ts = (takeTok ts) { tokenTok = KA KSemiColon }


takeTok :: [Lexeme n] -> Token (Tok n)
takeTok []      
 = Token (KJunk "") (SourcePos "" 0 0)

takeTok (l : ls)
 = case l of
        LexemeToken (Token { tokenTok = KM KNewLine })
         -> takeTok ls

        LexemeToken t           -> t
        LexemeStartLine  _      -> takeTok ls
        LexemeStartBlock _      -> takeTok ls
