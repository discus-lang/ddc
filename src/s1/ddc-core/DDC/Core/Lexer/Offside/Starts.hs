
module DDC.Core.Lexer.Offside.Starts
        (addStarts)
where
import DDC.Core.Lexer.Tokens
import DDC.Core.Lexer.Offside.Base
import DDC.Data.SourcePos


-- | Add block and line start tokens to this stream.
--
--   This is identical to the definition in the Haskell98 report,
--   except that we also use multi-token starting strings like
--   'imports' 'foreign' 'type'.
addStarts :: (Eq n, Show n) => [Located (Token n)] -> [Lexeme n]
addStarts ts
 -- If the first lexeme of a module is not '{' then start a new block.
 | t1 : tsRest  <- dropNewLines ts
 = if not $ or $ map (isToken t1) [KA (KSymbol SBraceBra)]
        then LexemeStartBlock (sourcePosOfLocated t1) (columnOfLocated t1)
                : addStarts' (t1 : tsRest)
        else addStarts' (t1 : tsRest)

        -- empty file
 | otherwise
 = []


addStarts'  :: Eq n => [Located (Token n)] -> [Lexeme n]
addStarts' ts
 -- Block started at end of input.
 | Just (ts1, ts2)      <- splitBlockStart ts
 , []                   <- dropNewLines ts2
 , t1 : _               <- ts1
 = [LexemeToken (sourcePosOfLocated t) (valueOfLocated t) | t <- ts1]
         ++ [LexemeStartBlock (sourcePosOfLocated t1) 0]

 -- Block start point where there is not an explicit open brace.
 --  Inject a StartBlock marker including the current column depth.
 | Just (ts1, ts2)       <- splitBlockStart ts
 , t2 : tsRest           <- dropNewLines ts2
 , not $ isToken t2 (KA (KSymbol SBraceBra))
 = [LexemeToken (sourcePosOfLocated t) (valueOfLocated t) | t <- ts1]
         ++ [LexemeStartBlock (sourcePosOfLocated t2) (columnOfLocated t2)]
         ++ addStarts' (t2 : tsRest)

 -- check for start of list
 | Located     l1 t1@(KA (KSymbol SBraceBra)) : ts' <- ts
 = LexemeToken l1 t1 : addStarts' ts'

 -- check for end of list
 | Located     l1 t1@(KA (KSymbol SBraceKet)) : ts' <- ts
 = LexemeToken l1 t1 : addStarts' ts'

 -- check for start of new line
 | t1 : ts'              <- ts
 , isToken t1 (KM KNewLine)
 , t2 : tsRest   <- dropNewLines ts'
 , not $ isToken t2 (KA (KSymbol SBraceBra))
 = LexemeStartLine (sourcePosOfLocated t2) (columnOfLocated t2)
         : addStarts' (t2 : tsRest)

 -- eat up trailine newlines
 | Located _l1 (KM KNewLine) : ts' <- ts
 = addStarts' ts'

 -- a regular token
 | Located     l1 t1 : ts' <- ts
 = LexemeToken l1 t1 : addStarts' ts'

 -- end of input
 | otherwise
 = []


-- | Drop newline tokens at the front of this stream.
dropNewLines :: Eq n => [Located (Token n)] -> [Located (Token n)]
dropNewLines []
 = []

dropNewLines (t1:ts)
 | isToken t1 (KM KNewLine)
 = dropNewLines ts

 | otherwise
 = t1 : ts


-- | Check if a token is one that starts a block of statements.
splitBlockStart
        :: [Located (Token n)]
        -> Maybe ([Located (Token n)], [Located (Token n)])

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

