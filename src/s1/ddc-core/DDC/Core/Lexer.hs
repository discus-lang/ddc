
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
import DDC.Core.Lexer.Token.Builtin
import DDC.Core.Lexer.Token.Index
import DDC.Core.Lexer.Token.Keyword
import DDC.Core.Lexer.Token.Literal
import DDC.Core.Lexer.Token.Names
import DDC.Core.Lexer.Token.Operator
import DDC.Core.Lexer.Token.Symbol

import DDC.Core.Lexer.Offside
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos
import Data.Text                                (Text)
import qualified System.IO.Unsafe               as System
import qualified Text.Lexer.Inchworm.Char       as I
import qualified Data.Text                      as Text


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
 = locatedOfLexemes
        $ applyOffside []
        $ addStarts
        $ lexemesOfLocated
        $ dropUnused
        $ lexText sourceName lineStart
        $ Text.pack str

 where  dropUnused ts
         = case ts of
                []                              -> []
                Located _ (KM KComment{}) : ts' -> dropUnused ts'
                t : ts'                         -> t : dropUnused ts'


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
 = dropUnused
        $ lexText sourceName lineStart
        $ Text.pack str

 where  dropUnused ts
         = case ts of
                []                              -> []
                Located _ (KM KComment{}) : ts' -> dropUnused ts'
                Located _ (KM KNewLine{}) : ts' -> dropUnused ts'
                t : ts'                         -> t : dropUnused ts'


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

lexText filePath nStart txt
 = let  (toks, locEnd, strLeftover)
         = System.unsafePerformIO
         $ I.scanListIO
                (I.Location nStart 1)
                 I.bumpLocationWithChar
                (Text.unpack txt)
                (scanner filePath)

        I.Location lineEnd colEnd = locEnd
        spEnd   = SourcePos filePath lineEnd colEnd

   in   case strLeftover of
         []     -> toks
         str    -> toks ++ [Located spEnd (KErrorJunk (take 10 str))]


-- | Scanner for core tokens tokens.
type Scanner a
        = I.Scanner IO I.Location [Char] a


-------------------------------------------------------------------------------
-- | Scanner for source and core files.
--
--   The lexical structure for source and core is a bit different,
--   but close enough that there's no point writing a separate lexer yet.
--
scanner :: FilePath
        -> Scanner (Located (Token String))

scanner fileName
 = let
        stamp   :: (I.Location, a) -> Located a
        stamp (I.Location line col, token)
         = Located (SourcePos fileName line col) token
        {-# INLINE stamp #-}

        stamp'  :: (a -> b)
                -> (I.Location, a) -> Located b
        stamp' k (I.Location line col, token)
          = Located (SourcePos fileName line col) (k token)
        {-# INLINE stamp' #-}

   in I.skip (\c -> c == ' ' || c == '\t')
        $ I.alts
        [ -- Newlines are scanned to their own tokens because
          -- the transform that manages the offside rule uses them.
          fmap stamp
           $ I.from     (\c -> case c of
                                '\n'        -> return $ KM KNewLine
                                _           -> Nothing)

          -- Scan comments into their own tokens,
          -- these then get dropped by the dropComments function.
        , fmap (stamp' (KM . KComment)) $ I.scanHaskellCommentLine
        , fmap (stamp' (KM . KComment)) $ I.scanHaskellCommentBlock

          -- deBruijn indices.
          --   Needs to come before scanSymbol as '^' is also an operator.
        , fmap (stamp' (KA . KIndex))   $ scanIndex

          -- Literal values.
        , fmap (stamp' (\(l, b) -> KA (KLiteral l b)))
           $ scanLiteral

          -- Infix operators.
          --   Needs to come before scanSymbol because operators
          --   like "==" are parsed atomically rather than as
          --   two separate '=' symbols.
        , fmap (stamp' (KA . KOp))      $ scanInfixOperator

          -- Prefix operators.
        , fmap (stamp' (KA . KOpVar))   $ scanPrefixOperator

          -- The unit value.
          --   Needs to come before scanSymbol because the "()"
          --   lexeme is parsed atomically rather than as
          --   separate '(' and ')' symbols.
        , fmap stamp
           $ I.froms    (Just 2)
                        (\ss -> if ss == "()"
                                then Just (KA $ KBuiltin $ BDaConUnit)
                                else Nothing)

          -- Symbolic tokens like punctuation.
        , fmap (stamp' (KA . KSymbol))  $ scanSymbol

          -- Named things.
          --   Keywords have the same lexical structure as variables as
          --   they all start with a lower-case letter. We need to check
          --   for keywords before accepting a variable.
        , fmap (stamp' (KA . KBuiltin)) $ scanBuiltin
        , fmap (stamp' (KA . KKeyword)) $ scanKeyword
        , fmap (stamp' (KN . KCon))     $ scanConName
        , fmap (stamp' (KN . KVar))     $ scanVarName
        ]

