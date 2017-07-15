
module DDC.Core.Lexer.Offside.Starts
        (addStarts)
where
import DDC.Core.Lexer.Tokens
import DDC.Core.Lexer.Offside.Base


-- | Add block and line start tokens to this stream.
--
--   This is identical to the definition in the Haskell98 report,
--   except that we also use multi-token starting strings like
--   'imports' 'foreign' 'type'.
addStarts
        :: (Eq n, Show n)
        => [Lexeme n] -> [Lexeme n]
addStarts ts
 -- If the first lexeme of a module is not '{' then start a new block.
 | t1 : tsRest  <- dropNewLinesLexeme ts
 = if not $ or $ map (isToken t1) [KA (KSymbol SBraceBra)]
        then LexemeStartBlock (sourcePosOfLexeme t1)
                : addStarts' (t1 : tsRest)
        else addStarts' (t1 : tsRest)

        -- empty file
 | otherwise
 = []


addStarts'
        :: Eq n
        => [Lexeme n] -> [Lexeme n]
addStarts' ts
 -- Block started at end of input.
 | Just (ts1, ts2)      <- splitBlockStart ts
 , []                   <- dropNewLinesLexeme ts2
 , t1 : _               <- ts1
 = ts1  ++ [LexemeStartBlock (sourcePosOfLexeme t1)]

 -- Block start point where there is not an explicit open brace.
 --  Inject a StartBlock marker including the current column depth.
 | Just (ts1, ts2)       <- splitBlockStart ts
 , t2 : tsRest           <- dropNewLinesLexeme ts2
 , not $ isToken t2 (KA (KSymbol SBraceBra))
 = ts1  ++ [LexemeStartBlock (sourcePosOfLexeme t2)]
        ++ addStarts' (t2 : tsRest)

 -- check for start of list
 | LexemeToken l1 t1@(KA (KSymbol SBraceBra)) : ts' <- ts
 = LexemeToken l1 t1 : addStarts' ts'

 -- check for end of list
 | LexemeToken l1 t1@(KA (KSymbol SBraceKet)) : ts' <- ts
 = LexemeToken l1 t1 : addStarts' ts'

 -- check for start of new line
 | t1 : ts'     <- ts
 , isToken t1 (KM KNewLine)
 , t2 : tsRest  <- dropNewLinesLexeme ts'
 , not $ isToken t2 (KA (KSymbol SBraceBra))
 = LexemeStartLine (sourcePosOfLexeme t2)
         : addStarts' (t2 : tsRest)

 -- eat up trailine newlines
 | LexemeToken _sp (KM KNewLine) : ts' <- ts
 = addStarts' ts'

 -- a regular token
 | t : ts' <- ts
 = t : addStarts' ts'

 -- end of input
 | otherwise
 = []


-- | Check if a token is one that starts a block of statements.
splitBlockStart :: [Lexeme n] -> Maybe ([Lexeme n], [Lexeme n])
splitBlockStart toks

 -- export type
 |  t1@(LexemeToken _ (KA (KKeyword EExport)))
  : t2@(LexemeToken _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- export value
 |  t1@(LexemeToken _ (KA (KKeyword EExport)))
  : t2@(LexemeToken _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- export foreign X value
 |  t1@(LexemeToken _ (KA (KKeyword EExport)))
  : t2@(LexemeToken _ (KA (KKeyword EForeign)))
  : t3
  : t4@(LexemeToken _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import type
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import value
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import data
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EData)))
  : ts
 <- toks = Just ([t1, t2], ts)

 -- import foreign X type
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EForeign)))
  : t3
  : t4@(LexemeToken _ (KA (KKeyword EType)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import foreign X capability
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EForeign)))
  : t3
  : t4@(LexemeToken _ (KA (KKeyword ECapability)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 -- import foreign X value
 |  t1@(LexemeToken _ (KA (KKeyword EImport)))
  : t2@(LexemeToken _ (KA (KKeyword EForeign)))
  : t3
  : t4@(LexemeToken _ (KA (KKeyword EValue)))
  : ts
 <- toks = Just ([t1, t2, t3, t4], ts)

 |  t1@(LexemeToken _ (KA (KKeyword EDo)))     : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword EOf)))     : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword ELetRec))) : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword EWhere)))  : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword EExport))) : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword EImport))) : ts    <- toks = Just ([t1], ts)
 |  t1@(LexemeToken _ (KA (KKeyword EMatch)))  : ts    <- toks = Just ([t1], ts)

 | otherwise
 = Nothing

