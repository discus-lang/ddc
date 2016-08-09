
-- | Parser combinator framework specialised to lexical analysis.
--   Tokens can be specified via simple fold functions, 
--   and we include baked in source location handling.
--
--   If you want to parse expressions instead of tokens then try
--   try the @parsec@ or @attoparsec@ packages, which have more
--   general purpose combinators.
--
--   Comes with matchers for standard lexemes like integers,
--   comments, and Haskell style strings with escape handling. 
--
--   No dependencies other than the Haskell 'base' library.
--
module Text.Lexer.Inchworm
        ( -- * Basic Types
          Source
        , Scanner

          -- * Generic Scanning
        , scanListIO

          -- ** Source Construction
        , makeListSourceIO

          -- ** Scanner Evaluation
        , scanSourceToList

          -- * Combinators

          -- ** Basic
        , satisfies,    skip

          -- ** Accept
        , accept,       accepts

          -- ** From
        , from,         froms

          -- ** Alternation
        , alt,          alts

          -- ** Munching
        , munchPred,    munchWord,      munchFold)
where
import Text.Lexer.Inchworm.Source
import Text.Lexer.Inchworm.Scanner
import Text.Lexer.Inchworm.Combinator


-- | Scan a list of generic input tokens in the IO monad,
--   returning the source location of the final input token, 
--   along with the remaining input.
--
--   NOTE: If you just want to scan a `String` of characters
--   use @scanStringIO@ from "Text.Lexer.Inchworm.Char"
--
scanListIO 
        :: Eq i
        => loc                   -- ^ Starting source location.
        -> (i -> loc -> loc)     -- ^ Function to bump the current location by one input token.
        -> [i]                   -- ^ List of input tokens.
        -> Scanner IO loc [i] a  -- ^ Scanner to apply.
        -> IO ([a], loc, [i])

scanListIO loc bump input scanner
 = do   src     <- makeListSourceIO loc bump input
        scanSourceToList src scanner
