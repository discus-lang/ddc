{-# LANGUAGE BangPatterns #-}
-- | Character based scanners.
module Text.Lexer.Inchworm.Char
        ( module Text.Lexer.Inchworm

          -- * Driver
        , scanStringIO

          -- * Locations
        , Location (..)
        , bumpLocationWithChar

          -- * Scanners
        , scanInteger
        , scanHaskellChar
        , scanHaskellString
        , scanHaskellCommentBlock
        , scanHaskellCommentLine)
where
import Text.Lexer.Inchworm
import Text.Lexer.Inchworm.Source
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Numeric                as Numeric


-- Driver ---------------------------------------------------------------------
-- | Scan a string in the IO monad.
scanStringIO
        :: String
        -> Scanner IO Location String a
        -> IO ([a], Location, String)

scanStringIO str scanner
 = scanListIO
        (Location 1 1)
        bumpLocationWithChar 
        str scanner


-- Locations ------------------------------------------------------------------
-- | Bump a location using the given character,
--   updating the line and column number as appropriate. 
--  
bumpLocationWithChar :: Char -> Location -> Location
bumpLocationWithChar c (Location line col)
 = case c of 
        '\n'    -> Location (line + 1) 1
        _       -> Location line (col + 1) 


-- Integers -------------------------------------------------------------------
-- | Scan a decimal integer, with optional @-@ and @+@ sign specifiers.
scanInteger 
        :: Monad m 
        => Scanner m loc [Char] (loc, Integer)

scanInteger 
 = munchPred Nothing matchInt acceptInt
 where
        matchInt  0 !c  
         = c == '-' || c == '+' || Char.isDigit c

        matchInt  _ !c          = Char.isDigit c

        acceptInt ('+' : cs)
         | null cs              = Nothing

        acceptInt ('-' : cs)
         | null cs              = Nothing

        acceptInt cs            = Just $ read cs

{-# SPECIALIZE INLINE
     scanInteger
        :: Scanner IO Location [Char] (Location, Integer)
  #-}

-- Strings --------------------------------------------------------------------
-- | Scan a literal string,    enclosed in double quotes.
-- 
--   We handle the escape codes listed in Section 2.6 of the Haskell Report, 
--   but not string gaps or the @&@ terminator.
--
scanHaskellString 
        :: Monad   m
        => Scanner m loc [Char] (loc, String)

scanHaskellString 
 = munchFold Nothing matchC (' ', True) acceptC
 where
        matchC 0 '\"' _                 = Just ('\"', True)
        matchC _  _  (_, False)         = Nothing

        matchC ix c  (cPrev, True)
         | ix < 1                       = Nothing
         | c == '"' && cPrev == '\\'    = Just ('"', True)
         | c == '"'                     = Just (c,   False)
         | otherwise                    = Just (c,   True)

        acceptC ('"' : cs)              
         = case decodeString cs of
                (str, ('"' : []))       -> Just str
                _                       -> Nothing

        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellString
        :: Scanner IO Location [Char] (Location, String)  
  #-}


-- Characters -----------------------------------------------------------------
-- | Scan a literal character, enclosed in single quotes.
--   
--   We handle the escape codes listed in Section 2.6 of the Haskell Report.
--
scanHaskellChar 
        :: Monad   m
        => Scanner m loc [Char] (loc, Char)

scanHaskellChar 
 = munchFold Nothing matchC (' ', True) acceptC
 where
        matchC 0 '\'' _                 = Just ('\'', True)
        matchC _  _  (_,     False)     = Nothing

        matchC ix c  (cPrev, True)
         | ix < 1                       = Nothing
         | c == '\'' && cPrev == '\\'   = Just ('\'', True)
         | c == '\''                    = Just (c,    False)
         | otherwise                    = Just (c,    True)

        acceptC ('\'' : cs)          
         = case readChar cs of
                Just (c, "\'")          -> Just c
                _                       -> Nothing

        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellChar
        :: Scanner IO Location [Char] (Location, Char)
  #-}


-- Comments -------------------------------------------------------------------
-- | Scan a Haskell block comment.
scanHaskellCommentBlock 
        :: Monad   m
        => Scanner m loc [Char] (loc, String)

scanHaskellCommentBlock
 = munchFold Nothing matchC (' ', True) acceptC
 where
        matchC 0 '{' _                  = Just ('{', True)
        matchC 1 '-' ('{', True)        = Just ('-', True)

        matchC _   _  (_,     False)    = Nothing

        matchC ix  c  (cPrev, True)
         | ix < 2                       = Nothing
         | cPrev == '-' && c == '}'     = Just ('}', False)
         | otherwise                    = Just (c,   True)

        acceptC cc@('{' : '-' : _)      = Just cc
        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellCommentBlock 
        :: Scanner IO Location [Char] (Location, String)
  #-}


-- | Scan a Haskell line comment.
scanHaskellCommentLine 
        :: Monad   m
        => Scanner m loc [Char] (loc, String)

scanHaskellCommentLine 
 = munchPred Nothing matchC acceptC
 where
        matchC 0 '-'     = True
        matchC 1 '-'     = True
        matchC _ '\n'    = False
        matchC ix _       
         | ix < 2       = False
         | otherwise    = True

        acceptC cs      = Just cs

{-# SPECIALIZE INLINE
     scanHaskellCommentLine
        :: Scanner IO Location [Char] (Location, String)
  #-}


-------------------------------------------------------------------------------
-- | Decode escape codes in a string.
decodeString :: String -> (String, String)
decodeString ss0
 = go [] ss0
 where
        go !acc []
         = (reverse acc, [])

        go !acc ss@('\"' : _)
         = (reverse acc, ss)

        go !acc ss@(c : cs)
         = case readChar ss of
                Just (c', cs')  -> go (c' : acc) cs'
                Nothing         -> go (c  : acc) cs


-- | Read a character literal, handling escape codes.
readChar :: String -> Maybe (Char, String)

-- Control characters defined by hex escape codes.
readChar ('\\' : 'x' : cs)
 | [(x, rest)]  <- Numeric.readHex cs   = Just (Char.chr x, rest)
 | otherwise                            = Nothing

-- Control characters defined by octal escape codes.
readChar ('\\' : 'o' : cs)
 | [(x, rest)]  <- Numeric.readOct cs   = Just (Char.chr x, rest)
 | otherwise                            = Nothing

-- Control characters defined by carret characters, like \^G
readChar ('\\' : '^' : c : rest)
 | c >= 'A' && c <= 'Z'                 = Just (Char.chr (Char.ord c - 1), rest)
 | c == '@'                             = Just (Char.chr 0,  rest)
 | c == '['                             = Just (Char.chr 27, rest)
 | c == '\\'                            = Just (Char.chr 28, rest)
 | c == ']'                             = Just (Char.chr 29, rest)
 | c == '^'                             = Just (Char.chr 30, rest)
 | c == '_'                             = Just (Char.chr 31, rest)

-- Control characters defined by decimal escape codes.
readChar ('\\' : cs)
 | (csDigits, csRest)   <- List.span Char.isDigit cs
 , not $ null csDigits
 = Just (Char.chr (read csDigits), csRest)

-- Control characters defined by ASCII escape codes.
readChar ('\\' : cs)                    
 = let  go [] = Nothing
        go ((str, c) : moar)
         = case List.stripPrefix str cs of
                Nothing                 -> go moar
                Just rest               -> Just (c, rest)

   in   go escapedChars

-- Just a regular character.
readChar (c : rest)                     = Just (c, rest)

-- Nothing to read.
readChar _                              = Nothing

escapedChars :: [(String, Char)]
escapedChars 
 =      [ ("a",   '\a'),   ("b", '\b'),     ("f",   '\f'),   ("n", '\n')
        , ("r",   '\r'),   ("t", '\t'),     ("v",   '\v'),   ("\\",  '\\')
        , ("\"",  '\"'),   ("\'",  '\'')
        , ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX')
        , ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL')
        , ("BS",  '\BS'),  ("HT",  '\HT'),  ("LF",  '\LF'),  ("VT",  '\VT')
        , ("FF",  '\FF'),  ("CR",  '\CR'),  ("SO",  '\SO'),  ("SI",  '\SI')
        , ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
        , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB')
        , ("CAN", '\CAN'), ("EM",  '\EM'),  ("SUB", '\SUB'), ("ESC", '\ESC')
        , ("FS",  '\FS'),  ("GS",  '\GS'),  ("RS",  '\RS'),  ("US",  '\US')
        , ("SP",  '\SP'),  ("DEL", '\DEL')]

