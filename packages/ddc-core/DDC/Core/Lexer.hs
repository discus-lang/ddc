
-- | Reference lexer for core langauge parser. Slow but Simple.
--
--   The lexers here all use 'String' in place of a real name type.
--   After applying these functions to the program text, we need
--   to use `renameTok` tok convert the strings in `TokNamed` tokens
--   into the name type specific to the langauge fragment to be parsed.
--
module DDC.Core.Lexer
        ( module DDC.Core.Lexer.Tokens
        , module DDC.Core.Lexer.Names

          -- * Lexer
        , lexModuleWithOffside
        , lexExp)
where
import DDC.Core.Lexer.Offside
import DDC.Core.Lexer.Comments
import DDC.Core.Lexer.Names
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos
import DDC.Data.Token
import Data.Char
import Data.Text                        (Text)
import qualified Data.Text              as T
import Data.Monoid


-- Module -----------------------------------------------------------------------------------------
-- | Lex a module and apply the offside rule.
--
--   Automatically drop comments from the token stream along the way.
--
lexModuleWithOffside 
        :: FilePath     -- ^ Path to source file, for error messages.
        -> Int          -- ^ Starting line number.
        -> String       -- ^ String containing program text.
        -> [Token (Tok String)]

lexModuleWithOffside sourceName lineStart str
 = {-# SCC lexWithOffside #-}
        applyOffside [] []
        $ addStarts
        $ dropComments 
        $ lexText sourceName lineStart 
        $ T.pack str


-- Exp --------------------------------------------------------------------------------------------
-- | Lex a string into tokens.
--
--   Automatically drop comments from the token stream along the way.
--
lexExp  :: FilePath     -- ^ Path to source file, for error messages.
        -> Int          -- ^ Starting line number.
        -> String       -- ^ String containing program text.
        -> [Token (Tok String)]

lexExp sourceName lineStart str
 = {-# SCC lexExp #-}
        dropNewLines
        $ dropComments
        $ lexText sourceName lineStart 
        $ T.pack str


-- Generic ----------------------------------------------------------------------------------------
-- Tokenize some input text.
--
-- NOTE: Although the main interface for the lexer uses standard Haskell strings,
--       we're using Text internally to get proper unicode tokenization.
--       Eventually, we should refactor the API to only pass around Text, rather
--       than Strings.
--
lexText :: String       -- ^ Name of source file, which is attached to the tokens.
        -> Int          -- ^ Starting line number.
        -> Text         -- ^ Text to tokenize.
        -> [Token (Tok String)]

lexText sourceName lineStart xx
 = let  sp      = SourcePos sourceName lineStart 1
   in   lexWord sp xx


lexWord :: SourcePos -> Text -> [Token (Tok String)]
lexWord sp@(SourcePos sourceName line column) w
 = match w
 where
        tok t = Token t (SourcePos sourceName line column)
        tokM  = tok . KM
        tokA  = tok . KA
        tokN  = tok . KN

        lexMore n rest
         = lexWord (SourcePos sourceName line (column + n)) rest

        lexUpto pat rest
         = case dropWhile (not . T.isPrefixOf pat) (T.tails rest) of
                x : _   -> x
                _       -> T.empty

        txt           = T.pack 
        prefix str    = T.stripPrefix (T.pack str)

        match cs
         | T.null cs
         = []

         -- Whitespace
         | Just (' ', rest)     <- T.uncons cs
         = lexMore 1 rest

         | Just ('\t', rest)    <- T.uncons cs
         = lexMore 8 rest

         -- Meta tokens
         | Just rest            <- T.stripPrefix (txt "{-#") cs
         , (prag, rest')        <- T.breakOn     (txt "#-}") rest
         , rest''               <- T.drop 3 rest'
         , len                  <- 3 + T.length prag + 3
         = tokA (KPragma prag)          : lexMore len rest''


         | Just rest            <- T.stripPrefix (txt "{-") cs
         = tokM KCommentBlockStart      : lexMore 2 (lexUpto (txt "-}") rest)

         | Just rest            <- T.stripPrefix (txt "-}") cs
         = tokM KCommentBlockEnd        : lexMore 2 rest

         | Just cs1             <- T.stripPrefix (txt "--") cs
         , (_junk, rest)        <- T.span (/= '\n') cs1
         = tokM KCommentLineStart       : lexMore 2 rest

         | Just ('\n', rest)    <- T.uncons cs
         = tokM KNewLine                : lexWord (SourcePos sourceName (line + 1) 1) rest

         -- Double character symbols.
         | not (T.compareLength cs 2 == LT)
         , (cs1, rest)          <- T.splitAt 2 cs
         , Just t      
            <- case T.unpack cs1 of
                "[:"            -> Just KSquareColonBra
                ":]"            -> Just KSquareColonKet
                "{:"            -> Just KBraceColonBra
                ":}"            -> Just KBraceColonKet
                "~>"            -> Just KArrowTilde
                "->"            -> Just KArrowDash
                "<-"            -> Just KArrowDashLeft
                "=>"            -> Just KArrowEquals
                "/\\"           -> Just KBigLambdaSlash
                "()"            -> Just KDaConUnit
                _               -> Nothing
         = tokA t : lexMore 2 rest

         -- Wrapped operator symbols.
         -- This needs to come before lexing single character symbols.
         | Just ('(', cs1)      <- T.uncons cs
         , Just (c,   cs2)      <- T.uncons cs1
         , isOpStart c
         , (body, cs3)          <- T.span isOpBody cs2
         , Just (')', rest)     <- T.uncons cs3
         = tokA (KOpVar (T.unpack (T.cons c body))) 
                                                : lexMore (2 + T.length (T.cons c body)) rest

         -- Literal numeric values
         -- This needs to come before the rule for '-'
         | Just (c, cs1)        <- T.uncons cs
         , isDigit c
         , (body, rest)         <- T.span isLitBody cs1
         = let  str             =  T.unpack (T.cons c body)
           in   tokN (KLit str) : lexMore (length str) rest

         | Just ('-', cs1)      <- T.uncons cs
         , Just (c,   _)        <- T.uncons cs1
         , isDigit c
         = let  (body, rest)   = T.span isLitBody cs1
                str            = T.unpack (T.cons '-' body)
           in   tokN (KLit str) : lexMore (length str) rest

         -- Lex a literal string.
         | Just (tk, sp', rest) <- lexLitString sp cs
         = tk : lexWord sp' rest

         -- Operator symbols.
         | Just (c, cs1)        <- T.uncons cs
         , isOpStart c
         , (body, rest)         <- T.span isOpBody cs1
         , sym                  <- T.cons c body
         , sym /= T.pack "="
         , sym /= T.pack "|"
         = tokA (KOp (T.unpack sym)) : lexMore (1 + T.length body) rest

         -- Single character punctuation.
         | Just (tk, sp', rest)  <- lexPunc sp cs
         = tk : lexWord sp' rest

         -- Debruijn indices
         | Just ('^', cs1)      <- T.uncons cs
         , (ds, rest)           <- T.span isDigit cs1
         , T.length ds >= 1
         = tokA (KIndex (read (T.unpack ds)))   : lexMore (1 + T.length ds) rest         
        
         -- Operator body symbols.
         | Just ('^', rest)     <- T.uncons cs
         = tokA KHat                            : lexMore 1 rest

         -- Bottoms
         | Just rest            <- prefix "Pure" cs
         = tokA KBotEffect                      : lexMore 4 rest

         | Just rest            <- prefix "Empty" cs
         = tokA KBotClosure                     : lexMore 5 rest

         -- Named Constructors
         | Just (c, cs1)        <- T.uncons cs
         , isConStart c
         , (body,  rest)        <- T.span isConBody cs1
         , (body', rest')       <- case T.uncons rest of
                                        Just ('\'', rest') -> (body <> T.pack "'", rest')
                                        Just ('#',  rest') -> (body <> T.pack "#", rest')
                                        _                  -> (body, rest)
         = let readNamedCon s
                 | Just socon   <- readSoConBuiltin s
                 = tokA (KSoConBuiltin socon)    : lexMore (length s) rest'

                 | Just kicon   <- readKiConBuiltin s
                 = tokA (KKiConBuiltin kicon)    : lexMore (length s) rest'

                 | Just twcon   <- readTwConBuiltin s
                 = tokA (KTwConBuiltin twcon)    : lexMore (length s) rest'
                 
                 | Just tccon   <- readTcConBuiltin s
                 = tokA (KTcConBuiltin tccon)    : lexMore (length s) rest'
                 
                 | Just con     <- readCon s
                 = tokN (KCon con)               : lexMore (length s) rest'
               
                 | otherwise    
                 = [tok (KErrorJunk [c])]
                 
            in  readNamedCon (T.unpack (T.cons c body'))

         -- Keywords, Named Variables and Witness constructors
         | Just (c, cs1)         <- T.uncons cs
         , isVarStart c
         , (body,  rest)         <- T.span isVarBody cs1
         , (body', rest')        <- case T.uncons rest of
                                        Just ('#', rest') -> (body <> T.pack "#", rest')
                                        _                 -> (body, rest)
         = let readNamedVar s
                 | "_"          <- s
                 = tokA KUnderscore        : lexMore (length s) rest'

                 | Just t       <- lookup s keywords
                 = tok t                   : lexMore (length s) rest'
         
                 | Just v       <- readVar s
                 = tokN (KVar v)           : lexMore (length s) rest'

                 | otherwise
                 = [tok (KErrorJunk [c])]

            in  readNamedVar (T.unpack (T.cons c body'))

         -- Some unrecognised character.
         | otherwise
         = case T.unpack cs of
                (c : _) -> [tok $ KErrorJunk [c]]
                _       -> [tok $ KErrorJunk []]



-------------------------------------------------------------------------------
-- | Lex a punctuation token.
--   These are lexed independently of any other following characters.
lexPunc :: SourcePos -> Text
        -> Maybe (Token (Tok String), SourcePos, Text)

lexPunc sp@(SourcePos name line col) tx
 | not (T.compareLength tx 2 == LT)
 , (cs1, rest)          <- T.splitAt 2 tx
 , Just t
   <- case T.unpack cs1 of
        "[:"            -> Just KSquareColonBra
        ":]"            -> Just KSquareColonKet
        "{:"            -> Just KBraceColonBra
        ":}"            -> Just KBraceColonKet
        _               -> Nothing
 = Just ( Token (KA t) sp
        , SourcePos name line (col + 2)
        , rest)

 | Just (c, rest) <- T.uncons tx
 , Just t           
   <- case c of
        '('             -> Just KRoundBra
        ')'             -> Just KRoundKet
        '['             -> Just KSquareBra
        ']'             -> Just KSquareKet
        '{'             -> Just KBraceBra
        '}'             -> Just KBraceKet
        '.'             -> Just KDot
        ','             -> Just KComma
        ';'             -> Just KSemiColon
        '@'             -> Just KAt
        '\\'            -> Just KBackSlash
        'λ'             -> Just KLambda
        'Λ'             -> Just KBigLambda
        '→'             -> Just KArrowDash
        '='             -> Just KEquals
        '|'             -> Just KBar

        _               -> Nothing
 = Just ( Token (KA t) sp
        , SourcePos name line (col + 1)
        , rest)

 -- No match.
 | otherwise
 = Nothing


-------------------------------------------------------------------------------
-- | Lex a literal string.
--   We force these to be null terminated so the representation
--   is compatable with C string functions.
lexLitString
        :: SourcePos -> Text
        -> Maybe (Token (Tok String), SourcePos, Text)

lexLitString sp@(SourcePos name line col) tx 
 | Just ('\"', cc)      <- T.uncons tx
 = let 
        eat n acc xs
         | Just ('\\', xs1)     <- T.uncons xs
         , Just ('"',  xs2)     <- T.uncons xs1
         = eat (n + 2) ('"' : acc) xs2

         | Just ('\\', xs1)     <- T.uncons xs
         , Just ('n',  xs2)     <- T.uncons xs1
         = eat (n + 2) ('\n' : acc) xs2

         | Just ('"',  xs1)     <- T.uncons xs
         = Just ( Token (KA (KString (T.pack (reverse acc)))) sp
                , SourcePos name line (col + n)
                , xs1)

         | Just (c,    xs1)     <- T.uncons xs
         = eat (n + 1) (c : acc) xs1

         | otherwise
         = Just ( Token (KErrorUnterm (T.unpack tx)) sp
                , sp, T.empty)

   in eat 0 [] cc

 | otherwise
 = Nothing

