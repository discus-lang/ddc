
module DDC.Core.Lexer.Comments
        ( dropComments
        , dropNewLines)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos


-- | Drop all the comments and newline tokens in this stream.
dropComments 
        :: Eq n => [Located (Tok n)] -> [Located (Tok n)]

dropComments []      = []
dropComments (t@(Located sourcePos tok) : xs)
 = case tok of
        KM KCommentLineStart 
         -> dropComments $ dropWhile (\t' -> not $ isToken t' (KM KNewLine)) xs

        KM KCommentBlockStart 
         -> dropComments $ dropCommentBlock sourcePos xs t

        _ -> t : dropComments xs


-- | Drop block comments form a token stream.
dropCommentBlock 
        :: Eq n
        => SourcePos            -- ^ Position of outer-most block comment start.
        -> [Located (Tok n)] 
        ->  Located (Tok n) 
        -> [Located (Tok n)]

dropCommentBlock spStart [] _terr
        = [Located spStart (KM KCommentUnterminated)]

dropCommentBlock spStart (t@(Located _ tok) : xs) terr
 = case tok of
        -- enter into nested block comments.
        KM KCommentBlockStart
         -> dropCommentBlock spStart (dropCommentBlock spStart xs t) terr

        -- outer-most block comment has ended.
        KM KCommentBlockEnd
         -> xs

        _ -> dropCommentBlock spStart xs terr


-- | Drop newline tokens from this list.
dropNewLines :: Eq n => [Located (Tok n)] -> [Located (Tok n)]
dropNewLines [] = []
dropNewLines (t:ts)
        | isToken t (KM KNewLine)
        = dropNewLines ts

        | otherwise
        = t : dropNewLines ts


isToken :: Eq n => Located (Tok n) -> Tok n -> Bool
isToken (Located _ tok) tok2 = tok == tok2

