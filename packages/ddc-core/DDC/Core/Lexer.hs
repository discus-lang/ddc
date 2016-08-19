
-- | Reference lexer for core langauge parser. Slow but Simple.
--
--   The lexers here all use 'String' in place of a real name type.
--   After applying these functions to the program text, we need
--   to use `renameTok` tok convert the strings in `TokNamed` tokens
--   into the name type specific to the langauge fragment to be parsed.
--
module DDC.Core.Lexer
        ( module DDC.Core.Lexer.Tokens
        , Located (..)

          -- * Lexer
        , lexModuleWithOffside
        , lexExp)
where
import DDC.Core.Lexer.Token.Literal
import DDC.Core.Lexer.Token.Builtin
import DDC.Core.Lexer.Token.Keyword
import DDC.Core.Lexer.Token.Operator
import DDC.Core.Lexer.Token.Names
import DDC.Core.Lexer.Token.Symbol
import DDC.Core.Lexer.Offside
import DDC.Core.Lexer.Comments
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos
import Data.Char
import Data.Text                        (Text)
import qualified Data.Text              as T


-- Module -----------------------------------------------------------------------------------------
-- | Lex a module and apply the offside rule.
--
--   Automatically drop comments from the token stream along the way.
--
lexModuleWithOffside 
        :: FilePath     -- ^ Path to source file, for error messages.
        -> Int          -- ^ Starting line number.
        -> String       -- ^ String containing program text.
        -> [Located (Token String)]

lexModuleWithOffside sourceName lineStart str
 = applyOffside [] []
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
        -> [Located (Token String)]

lexExp sourceName lineStart str
 = dropNewLines
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
        -> [Located (Token String)]

lexText sourceName lineStart xx
 = let  sp      = SourcePos sourceName lineStart 1
   in   lexWord sp xx


lexWord :: SourcePos
        -> Text 
        -> [Located (Token String)]

lexWord sp@(SourcePos sourceName line column) w
 = match w
 where
        tok t = Located sp t
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

         -- Unit constructor ().
         | not (T.compareLength cs 2 == LT)
         , (cs1, rest)          <- T.splitAt 2 cs
         , T.unpack cs1 == "()"
         = tokA (KBuiltin BDaConUnit) : lexMore 2 rest

         -- Double character symbols.
         | not (T.compareLength cs 2 == LT)
         , (cs1, rest)          <- T.splitAt 2 cs
         , Just s               <- acceptSymbol2 (T.unpack cs1)
         = tokA (KSymbol s) : lexMore 2 rest

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
         , c == '-'
         , (body, rest)         <- T.span isLitBody cs1
         , str                  <- T.unpack (T.cons c body)
         , Just (lit, u)        <- acceptLiteral str
         = tokA (KLiteral lit u)  : lexMore (length str) rest

         | Just (c, cs1)        <- T.uncons cs
         , isDigit c
         , (body, rest)         <- T.span isLitBody cs1
         , str                  <- T.unpack (T.cons c body)
         , Just (lit, u)        <- acceptLiteral str
         = tokA (KLiteral lit u)  : lexMore (length str) rest

         -- Lex a literal string.
         | Just (tk, sp', rest) <- lexLitString sp cs
         = tk : lexWord sp' rest

         -- Operator symbols.
         | Just (c, cs1)        <- T.uncons cs
         , isOpStart c
         , (body, rest)         <- T.span isOpBody cs1
         , sym                  <- T.cons c body
         , Just str             <- acceptInfixOperator (T.unpack sym)
         = tokA (KOp str) : lexMore (length str) rest

         -- Debruijn indices
         | Just ('^', cs1)      <- T.uncons cs
         , (ds, rest)           <- T.span isDigit cs1
         , T.length ds >= 1
         = tokA (KIndex (read (T.unpack ds)))   : lexMore (1 + T.length ds) rest         

         -- Single character punctuation.
         | Just (tk, sp', rest) <- lexPunc sp cs
         = tk : lexWord sp' rest

         -- Named Constructors
         | Just (c, cs1)        <- T.uncons cs
         , isConStart c
         , (body,  rest)        <- T.span isConBody cs1
         , sym                  <- T.cons c body
         = let  readNamedCon s
                 | Just bb      <- acceptBuiltin s
                 = tokA (KBuiltin bb)           : lexMore (length s) rest
                 
                 | Just con     <- acceptConName s
                 = tokN (KCon con)              : lexMore (length s) rest
               
                 | otherwise    
                 = [tok (KErrorJunk [c])]
                 
            in  readNamedCon (T.unpack sym)

         -- Keywords, Named Variables and Witness constructors
         | Just (c, cs1)        <- T.uncons cs
         , isVarStart c
         , (body,  rest)        <- T.span isVarBody cs1
         , sym                  <- T.cons c body
         = let readNamedVar s
                 | "_"          <- s
                 = tokA (KSymbol SUnderscore) : lexMore (length s) rest

                 | Just k       <- lookup s keywords
                 = tokA (KKeyword k)          : lexMore (length s) rest
         
                 | Just v       <- acceptVarName s
                 = tokN (KVar v)              : lexMore (length s) rest

                 | otherwise
                 = [tok (KErrorJunk [c])]

            in  readNamedVar (T.unpack sym)


         -- Some unrecognised character.
         | otherwise
         = case T.unpack cs of
                (c : _) -> [tok $ KErrorJunk [c]]
                _       -> [tok $ KErrorJunk []]



-------------------------------------------------------------------------------
-- | Lex a punctuation token.
--   These are lexed independently of any other following characters.
lexPunc :: SourcePos -> Text
        -> Maybe (Located (Token String), SourcePos, Text)

lexPunc sp@(SourcePos name line col) tx
 | not (T.compareLength tx 2 == LT)
 , (cs1, rest)    <- T.splitAt 2 tx
 , Just s         <- acceptSymbol2 (T.unpack cs1)
 = Just ( Located   sp (KA (KSymbol s))
        , SourcePos name line (col + 2)
        , rest)

 | Just (c, rest) <- T.uncons tx
 , Just s         <- acceptSymbol1 c
 = Just ( Located   sp (KA (KSymbol s))
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
        -> Maybe (Located (Token String), SourcePos, Text)

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
         , Just ('#',  xs2)     <- T.uncons xs1
         = Just ( Located   sp (KA (KLiteral (LString (T.pack (reverse acc))) True))
                , SourcePos name line (col + n)
                , xs2)

         | Just ('"',  xs1)     <- T.uncons xs
         = Just ( Located   sp (KA (KLiteral (LString (T.pack (reverse acc))) False))
                , SourcePos name line (col + n)
                , xs1)

         | Just (c,    xs1)     <- T.uncons xs
         = eat (n + 1) (c : acc) xs1

         | otherwise
         = Just ( Located sp (KErrorUnterm (T.unpack tx))
                , sp, T.empty)

   in eat 0 [] cc

 | otherwise
 = Nothing

