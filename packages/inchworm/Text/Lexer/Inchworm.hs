
-- | Parser combinator framework specialized to lexical analysis.
--   Tokens can be specified via simple fold functions, 
--   and we include baked in source location handling.
--
--   If you want to parse expressions instead of performing lexical
--   analysis then try the @parsec@ or @attoparsec@ packages, which
--   have more general purpose combinators.
--
--   Matchers for standard tokens like comments and strings 
--   are in the "Text.Lexer.Inchworm.Char" module.
--
--   No dependencies other than the Haskell 'base' library.
--
-- __ Minimal example __
--
-- The following code demonstrates how to perform lexical analysis
-- of a simple LISP-like language. We use two separate name classes,
-- one for variables that start with a lower-case letter, 
-- and one for constructors that start with an upper case letter. 
--
-- Integers are scanned using the `scanInteger` function from the 
-- "Text.Lexer.Inchworm.Char" module.
--
-- The result of @scanStringIO@ contains the list of leftover input
-- characters that could not be parsed. In a real lexer you should
-- check that this is empty to ensure there has not been a lexical
-- error.
--
-- @
-- import Text.Lexer.Inchworm.Char
-- import qualified Data.Char      as Char
-- 
-- -- | A source token.
-- data Token 
--         = KBra | KKet | KVar String | KCon String | KInt Integer
--         deriving Show
-- 
-- -- | A thing with attached location information.
-- data Located a
--         = Located FilePath Location a
--         deriving Show
-- 
-- -- | Scanner for a lispy language.
-- scanner :: FilePath
--         -> Scanner IO Location [Char] (Located Token)
-- scanner fileName
--  = skip Char.isSpace
--  $ alts [ fmap (stamp id)   $ accept '(' KBra
--         , fmap (stamp id)   $ accept ')' KKet
--         , fmap (stamp KInt) $ scanInteger 
--         , fmap (stamp KVar)
--           $ munchWord (\\ix c -> if ix == 0 then Char.isLower c
--                                            else Char.isAlpha c) 
--         , fmap (stamp KCon) 
--           $ munchWord (\\ix c -> if ix == 0 then Char.isUpper c
--                                            else Char.isAlpha c)
--         ]
--  where  -- Stamp a token with source location information.
--         stamp k (l, t) 
--           = Located fileName l (k t)
-- 
-- main 
--  = do   let fileName = "Source.lispy"
--         let source   = "(some (Lispy like) 26 Program 93 (for you))"
--         toks    <- scanStringIO source (scanner fileName)
--         print toks
-- @
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
