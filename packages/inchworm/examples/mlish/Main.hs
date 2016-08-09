
import Scanner.Named
import Scanner.Operator
import Scanner.Punctuation
import Token
import Text.Lexer.Inchworm
import Text.Lexer.Inchworm.Char
import qualified Data.Char      as Char

main
 = do   let fileName  = "Source.mlish"
        let source
                = unlines
                [ "(box (run {:de:} {- this is a comment -} -1234 + 37))"
                , "-- derp"
                , "let junk = 35 in junk + junk"
                , "'d', 'e', 'r', 'p'"
                , "'\\n', '\\a', '\\''"
                , "'\\BEL'"
                , "'\\137'"
                , "\"derpo\\ntro\\BELnic\""]

        result  <- scanStringIO source (scanner fileName)
        print result


scanner fileName
 = skip (\c -> c == ' ' || c == '\t')
 $ alts [ 
          -- Block comments.
          fmap (stamp' KComment) $ scanHaskellCommentBlock

          -- Line comments.
        , fmap (stamp' KComment) $ scanHaskellCommentLine

          -- New line characters.
        , fmap stamp $ scanNewLine

          -- Punctuation.
        , fmap stamp $ scanPunctuation

          -- Named keywords, variables and constructors.
          -- These are handled in one scanner because keyword names like
          -- 'box' have the same lexical structure as variable names.
        , fmap stamp $ scanNamed

          -- Literal integers.
          -- Needs to come before scanOp so we don't take '-' independently.
        , fmap (stamp' (KLit . LInteger)) $ scanInteger

          -- Literal characters.
        , fmap (stamp' (KLit . LChar))    $ scanHaskellChar

          -- Literal strings.
        , fmap (stamp' (KLit . LString))  $ scanHaskellString

          -- Operator names.
          -- Needs to come after scanInteger so we don't take '-' independently.
        , fmap stamp $ scanOperator
        ]
 where  
        stamp  (l, t)
         = Located fileName l t

        stamp' k (l, t)
         = Located fileName l (k t)


scanNewLine :: Scanner IO Location [Char] (Location, Token)
scanNewLine 
 = accept '\n' KNewLine




