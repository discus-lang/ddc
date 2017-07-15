
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
splitBlockStart ls
 = go1 ls
 where
    go1 (l1@(LexemeToken _ t1) : ls1)
        | isKeyword t1 EExport || isKeyword t1 EImport
        = case ls1 of
                l2@(LexemeToken _ t2) : ls2
                 |  isKeyword t1 EExport
                 ,  isKeyword t2 EType || isKeyword t2 EValue
                 -> Just ([l1, l2], ls2)

                 |  isKeyword t1 EImport
                 ,  isKeyword t2 EType || isKeyword t2 EValue || isKeyword t2 EData
                 -> Just ([l1, l2], ls2)

                 |  otherwise
                 -> case ls2 of
                        l3@(LexemeToken _ _t3) : l4@(LexemeToken _ t4) : ls4
                         |  isKeyword t1 EExport
                         ,  isKeyword t2 EForeign
                         ,  isKeyword t4 EValue
                         -> Just ([l1, l2, l3, l4], ls4)

                         |  isKeyword t1 EImport
                         ,  isKeyword t2 EForeign
                         ,  isKeyword t4 EType || isKeyword t4 EValue || isKeyword t4 ECapability
                         -> Just ([l1, l2, l3, l4], ls4)

                        _ -> go2 ls

                _ -> go2 ls

    go1 _ = go2 ls


    go2 (l1@(LexemeToken _ (KA (KKeyword EDo)))     : ls1) = Just ([l1], ls1)
    go2 (l1@(LexemeToken _ (KA (KKeyword EOf)))     : ls1) = Just ([l1], ls1)
    go2 (l1@(LexemeToken _ (KA (KKeyword EWhere)))  : ls1) = Just ([l1], ls1)
    go2 (l1@(LexemeToken _ (KA (KKeyword EExport))) : ls1) = Just ([l1], ls1)
    go2 (l1@(LexemeToken _ (KA (KKeyword EImport))) : ls1) = Just ([l1], ls1)

    go2 _ = Nothing

