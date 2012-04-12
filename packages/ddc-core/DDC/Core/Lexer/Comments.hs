
module DDC.Core.Lexer.Comments
        (dropComments)
where
import DDC.Base.Lexer
import DDC.Core.Lexer.Tokens


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

        KM KNewLine
         -> dropComments xs

        _ -> t : dropComments xs


dropCommentBlock 
        :: Eq n
        => [Token (Tok n)] -> Token (Tok n) -> [Token (Tok n)]

dropCommentBlock [] _terr
        = error "unterminated comment block"

dropCommentBlock (t@(Token tok _) : xs) terr
 = case tok of
        -- enter into nested block comments.
        KM KCommentBlockStart
         -> dropCommentBlock (dropCommentBlock xs t) terr

        -- outer-most block comment has ended.
        KM KCommentBlockEnd
         -> xs

        _ -> dropCommentBlock xs terr


isToken :: Eq n => Token (Tok n) -> Tok n -> Bool
isToken (Token tok _) tok2 = tok == tok2
