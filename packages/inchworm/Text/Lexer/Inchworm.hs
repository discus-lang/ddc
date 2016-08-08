
-- | Simple lexer framework and combinators that allow the
--   lexemes to be specified via fold functions.
--
--   Contains just enough functionality to get the lexing job done, 
--   which makes the library small and easy to bootstrap. If you want
--   more combinators then try the @parsec@ or @attoparsec@ packages.
--
--   Comes with matchers for standard lexemes like integers,
--   comments, and Haskell style strings with escape handling. 
--
--   No dependencies other than the Haskell 'base' library.
--
module Text.Lexer.Inchworm
        ( -- * Basic Types
          Source   (..)
        , Scanner  (..)
        , Sequence (..)

          -- * Source Construction
        , makeListSourceIO

          -- * Scanner Evaluation
        , scanSourceToList

          -- * Combinators
        , satisfies,    skip
        , accept,       accepts
        , from,         froms
        , alt,          alts

          -- * Munching
        , munchPred,    munchFold)
where
import Text.Lexer.Inchworm.Source
import Text.Lexer.Inchworm.Scanner
import Text.Lexer.Inchworm.Combinator


