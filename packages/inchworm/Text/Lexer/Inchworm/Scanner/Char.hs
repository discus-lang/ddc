{-# LANGUAGE BangPatterns #-}
module Text.Lexer.Inchworm.Scanner.Char
        ( scanInteger
        , scanHaskellChar
        , scanHaskellString
        , scanHaskellCommentBlock
        , scanHaskellCommentLine)
where
import Text.Lexer.Inchworm
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Numeric                as Numeric


-- Integers -------------------------------------------------------------------
-- | Scan a literal integer, with optional '-' and '+' sign specifiers.
scanInteger :: Scanner IO [Char] Integer
scanInteger 
 = munchPred Nothing matchInt acceptInt
 where
        matchInt  0 c  
         = c == '-' || c == '+' || Char.isDigit c

        matchInt  _ c           = Char.isDigit c

        acceptInt ('+' : cs)
         | null cs              = Nothing

        acceptInt ('-' : cs)
         | null cs              = Nothing

        acceptInt cs            = Just $ read cs


-- Strings --------------------------------------------------------------------
-- | Scan a literal string,    enclosed in double quotes.
-- 
--   We handle the escape codes listed in Section 2.6 of the Haskell Report, 
--   but not string gaps.
--
scanHaskellString :: Scanner IO [Char] String
scanHaskellString 
 = munchFold Nothing match (' ', True) accept
 where
        match 0 '\"' _                  = Just ('\"', True)
        match _ c   (_, False)          = Nothing

        match ix c  (cPrev, True)
         | ix < 1                       = Nothing
         | c == '"' && cPrev == '\\'    = Just ('"', True)
         | c == '"'                     = Just (c,   False)
         | otherwise                    = Just (c,   True)

        accept ('"' : cs)              
         = case decodeString cs of
                (str, ('"' : []))       -> Just str
                _                       -> Nothing


-- Characters -----------------------------------------------------------------
-- | Scan a literal character, enclosed in single quotes.
--   
--   We handle the escape codes listed in Section 2.6 of the Haskell Report.
--
scanHaskellChar :: Scanner IO [Char] Char
scanHaskellChar 
 = munchFold Nothing match (' ', True) accept
 where
        match 0 '\'' _                  = Just ('\'', True)
        match _ c   (_,     False)      = Nothing

        match ix c  (cPrev, True)
         | ix < 1                       = Nothing
         | c == '\'' && cPrev == '\\'   = Just ('\'', True)
         | c == '\''                    = Just (c,    False)
         | otherwise                    = Just (c,    True)

        accept ('\'' : cs)          
         = case readChar cs of
                Just (c, "\'")          -> Just c
                _                       -> Nothing

        accept _                        = Nothing


-- Comments -------------------------------------------------------------------
-- | Scan a Haskell block comment.
scanHaskellCommentBlock :: Scanner IO [Char] String
scanHaskellCommentBlock
 = munchFold Nothing match (' ', True) accept
 where
        match 0 '{' _                   = Just ('{', True)
        match 1 '-' ('{', True)         = Just ('-', True)

        match _  c  (_,     False)      = Nothing

        match ix  c  (cPrev, True)
         | ix < 2                       = Nothing
         | cPrev == '-' && c == '}'     = Just ('}', False)
         | otherwise                    = Just (c,   True)

        accept cc@('{' : '-' : cs)      = Just cc
        accept _                        = Nothing


-- | Scan a Haskell line comment.
scanHaskellCommentLine :: Scanner IO [Char] String
scanHaskellCommentLine 
 = munchPred Nothing match accept
 where
        match 0 '-'     = True
        match 1 '-'     = True
        match _ '\n'    = False
        match ix _       
         | ix < 2       = False
         | otherwise    = True

        accept cs       = Just cs


-------------------------------------------------------------------------------
-- | Decode escape codes in a string.
decodeString :: String -> (String, String)
decodeString ss0
 = go [] ss0
 where
        go !acc []
         = (reverse acc, [])

        go !acc ss@('\"' : cs)
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

