
import Text.Lexer.Inchworm
import Text.Lexer.Inchworm.Scanner.Char
import qualified Data.Char      as Char


main
 = do   ss      <- makeListSourceIO 
                $ unlines
                [ "(box (run {:de:} {- this is a comment -} -1234 + 37))"
                , "-- derp"
                , "let junk = 35 in junk + junk"
                , "'d', 'e', 'r', 'p'"
                , "'\\n', '\\a', '\\''"
                , "'\\BEL'"
                , "'\\137'"
                , "\"derpo\\ntro\\BELnic\""]

        result  <- scanSourceToList ss scanner
        print result


-------------------------------------------------------------------------------
data Token
        = KNewLine
        | KComment      String
        | KPunc         String
        | KKeyWord      String
        | KVar          String
        | KCon          String
        | KOp           String
        | KLit          Lit
        deriving Show

data Lit
        = LInteger      Integer
        | LChar         Char
        | LString       String
        deriving Show


scanner
 = skip (\c -> c == ' ' || c == '\t')
 $ alts [ -- Block comments
          fmap KComment $ scanHaskellCommentBlock

          -- Line comments
        , fmap KComment $ scanHaskellCommentLine

          -- New line characters
        , scanNewLine

          -- Punctuation.
        , scanPunc

          -- Keywords and variables.
          -- These are handled in one scanner because keyword names like
          -- 'box' have the same lexical structure as variable names.
        , scanKeyVar

          -- Constructor names.
        , scanCon

          -- Literal integers.
          -- Needs to come before scanOp    so we don't take '-' independently.
        , fmap (KLit . LInteger) scanInteger

          -- Literal characters.
        , fmap (KLit . LChar)    scanHaskellChar

          -- Literal strings.
        , fmap (KLit . LString)  scanHaskellString

          -- Operator names.
          -- Needs to come after LitInteger so we don't take '-' independently.
        , scanOp
        ]


-- Newlines -------------------------------------------------------------------
scanNewLine :: Scanner IO [Char] Token
scanNewLine 
 = accept '\n' KNewLine


-- Punctuation ----------------------------------------------------------------
puncs1 
 =      [ '(', ')'
        , '[', ']'
        , '{', '}'
        , '.', ',', ';' ]

puncs2 
 =      [ "[:", ":]", "{:", ":}" ]

-- | Scan a punctuation character.
scanPunc  :: Scanner IO [Char] Token
scanPunc   
 = alt  (munchPred (Just 2) matchPunc2  acceptPunc2)
        (from               acceptPunc1)
 where  
        acceptPunc1 c
         | elem c puncs1        = Just $ KPunc [c]
         | otherwise            = Nothing

        matchPunc2 0 c  = elem c ['[', '{', ':']
        matchPunc2 1 c  = elem c [']', '}', ':']
        matchPunc2 _ _  = False

        acceptPunc2 cs
                | elem cs puncs2        = Just $ KPunc cs
                | otherwise             = Nothing


-- Variables ------------------------------------------------------------------
keywords
 =      [ "import",     "export" 
        , "box",        "run"
        , "let",        "in"
        , "case",       "of"
        , "do"
        , "if",         "then",         "else"]


-- | Scan a keyword or variable.
scanKeyVar   :: Scanner IO [Char] Token
scanKeyVar
 = munchPred Nothing matchKeyVar acceptKeyVar
 where
        matchKeyVar  :: Int -> Char -> Bool
        matchKeyVar 0 c    = isVarStart c
        matchKeyVar _ c    = isVarBody  c

        acceptKeyVar :: [Char] -> Maybe Token
        acceptKeyVar cs
                | elem cs keywords      = Just $ KKeyWord cs
                | otherwise             = Just $ KVar     cs


isVarStart :: Char -> Bool
isVarStart c
 =  Char.isLower c
 || c == '?'

isVarBody  :: Char -> Bool
isVarBody c
 =  Char.isAlpha c
 || Char.isDigit c
 || c == '_' || c == '\'' || c == '$' || c == '#'


-- Constructors ---------------------------------------------------------------
scanCon :: Scanner IO [Char] Token
scanCon
 = munchPred Nothing matchCon acceptCon
 where  
        matchCon 0 c    = isConStart c
        matchCon _ c    = isConBody  c

        acceptCon cs    = Just $ KCon cs


-- | Character can start a constructor name
isConStart :: Char -> Bool
isConStart c
 = Char.isUpper c


-- | Character can be in the body of a constructor name.
isConBody  :: Char -> Bool
isConBody c
 =  Char.isAlpha c
 || Char.isDigit c 
 || c == '_' || c == '\'' || c == '#'


-- Operators ------------------------------------------------------------------
scanOp :: Scanner IO [Char] Token
scanOp 
 = munchPred Nothing matchOp acceptOp
 where
        matchOp 0 c     = isOpStart c
        matchOp _ c     = isOpBody  c

        acceptOp cs             = Just $ KOp cs


-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'


-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'     || c == '^'     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'

