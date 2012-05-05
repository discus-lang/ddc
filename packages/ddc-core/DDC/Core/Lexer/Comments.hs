
module DDC.Core.Lexer.Comments
        ( dropComments
        , dropNewLines)
where
import DDC.Core.Lexer.Tokens
import DDC.Data.Token


-- | Drop all the comments and newline tokens in this stream.
dropComments 
        :: Eq n => [Token (Tok n)] -> [Token (Tok n)]

dropComments []      = []
dropComments (t@(Token tok _) : xs)
 = case tok of
        KM KCommentLineStart 
         -> dropComments $ dropWhile (\t' -> not $ isToken t' (KM KNewLine)) xs

        KM KCommentBlockStart 
         -> dropComments $ dropCommentBlock xs t

        _ -> t : dropComments xs


-- | Drop block comments form a token stream.
dropCommentBlock 
        :: Eq n
        => [Token (Tok n)] 
        -> Token (Tok n) 
        -> [Token (Tok n)]

dropCommentBlock [] _terr
        = error "DDC.Core.Lexer.Comments.dropCommentBlock: unterminated comment block"

dropCommentBlock (t@(Token tok _) : xs) terr
 = case tok of
        -- enter into nested block comments.
        KM KCommentBlockStart
         -> dropCommentBlock (dropCommentBlock xs t) terr

        -- outer-most block comment has ended.
        KM KCommentBlockEnd
         -> xs

        _ -> dropCommentBlock xs terr


-- | Drop newline tokens from this list.
dropNewLines :: Eq n => [Token (Tok n)] -> [Token (Tok n)]
dropNewLines [] = []
dropNewLines (t:ts)
        | isToken t (KM KNewLine)
        = dropNewLines ts

        | otherwise
        = t : dropNewLines ts


isToken :: Eq n => Token (Tok n) -> Tok n -> Bool
isToken (Token tok _) tok2 = tok == tok2

