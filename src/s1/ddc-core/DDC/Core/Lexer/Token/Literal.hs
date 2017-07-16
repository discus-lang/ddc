
module DDC.Core.Lexer.Token.Literal
        ( Literal       (..)
        , scanLiteral

        , isLitName
        , isLitStart
        , isLitBody

        , readLitInteger
        , readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits
        , readBinary
        , readHex)
where
import DDC.Core.Exp.Literal
import Text.Lexer.Inchworm.Char
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Data.Text              as Text


-- Literal --------------------------------------------------------------------
scanLiteral   :: Scanner IO Location [Char] (Location, (Literal, Bool))
scanLiteral
 = alts [ munchPred Nothing matchLiteral acceptLiteral

          -- Character literals with Haskell style escape codes.
        , do    (loc, c)    <- scanHaskellChar
                alts [ do _  <- satisfies (\c' -> c' == '#')
                          return (loc, (LChar  c, True))

                     , do return (loc, (LChar  c, False)) ]

          -- String literals with Haskell style escape codes.
        , do    (loc, str)  <- scanHaskellString
                alts [ do _ <- satisfies (\c -> c == '#')
                          return (loc, (LString (Text.pack str), True))

                     , do return (loc, (LString (Text.pack str), False))
                     ]
        ]


-- | Match a literal character.
matchLiteral  :: Int -> Char -> Bool
matchLiteral 0 c = isLitStart c
matchLiteral _ c = isLitBody c


-- | Accept a literal.
acceptLiteral :: String -> Maybe (Literal, Bool)
acceptLiteral str
 | ('#' : str') <- reverse str
 , Just lit     <- acceptLit  (reverse str')
 = Just (lit, True)

 | Just lit     <- acceptLit str
 = Just (lit, False)

 | otherwise
 = Nothing
 where acceptLit str'
        | Just i        <- readLitNat         str'      = Just (LNat   i)
        | Just i        <- readLitInt         str'      = Just (LInt   i)
        | Just i        <- readLitSize        str'      = Just (LSize  i)
        | Just (u, b)   <- readLitWordOfBits  str'      = Just (LWord  u b)
        | Just (f, b)   <- readLitFloatOfBits str'      = Just (LFloat f b)
        | otherwise                                     = Nothing


-------------------------------------------------------------------------------
-- | String is the name of a literal.
isLitName :: String -> Bool
isLitName str
 = case str of
        []      -> False
        c : cs
         | isLitStart c
         , and (map isLitBody cs)
         -> True

         | otherwise
         -> False


-- | Character can start a literal.
isLitStart :: Char -> Bool
isLitStart c
        =   Char.isDigit c
        ||  c == '-'


-- | Character can be part of a literal body.
isLitBody :: Char -> Bool
isLitBody c
        =  Char.isDigit c
        || c == 'b' || c == 'o' || c == 'x'
        || c == 'w' || c == 'f' || c == 'i' || c == 's'
        || c == '.'
        || c == '#'
        || c == '\''



-------------------------------------------------------------------------------
-- | Read a signed integer.
readLitInteger :: String -> Maybe Integer
readLitInteger []       = Nothing
readLitInteger str@(c:cs)
        | '-'           <- c
        , all Char.isDigit cs
        = Just $ read str

        | all Char.isDigit cs
        = Just $ read str

        | otherwise
        = Nothing


-- | Read an integer with an explicit format specifier like @1234i@.
readLitNat :: String -> Maybe Integer
readLitNat str1
        | (ds, "")      <- List.span Char.isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an integer literal with an explicit format specifier like @1234i@.
readLitInt :: String -> Maybe Integer
readLitInt str1
        | '-' : str2    <- str1
        , (ds, "i")     <- List.span Char.isDigit str2
        , not  $ null ds
        = Just $ negate $ read ds

        | (ds, "i")     <- List.span Char.isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an size literal with an explicit format specifier like @1234s@.
readLitSize :: String -> Maybe Integer
readLitSize str1
        | '-' : str2    <- str1
        , (ds, "s")     <- List.span Char.isDigit str2
        , not  $ null ds
        = Just $ negate $ read ds

        | (ds, "s")     <- List.span Char.isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read a word with an explicit format speficier.
readLitWordOfBits :: String -> Maybe (Integer, Int)
readLitWordOfBits str1
        -- binary like 0b01001w32
        | Just str2     <- List.stripPrefix "0b" str1
        , (ds, str3)    <- List.span (\c -> c == '0' || c == '1') str2
        , not $ null ds
        , Just str4     <- List.stripPrefix "w" str3
        , (bs, "")      <- List.span Char.isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readBinary ds, bits)

        -- hex like 0x0ffw32
        | Just str2     <- List.stripPrefix "0x" str1
        , (ds, str3)    <- List.span (\c -> elem c ['0' .. '9']
                                         || elem c ['A' .. 'F']
                                         || elem c ['a' .. 'f']) str2
        , not $ null ds
        , Just str4     <- List.stripPrefix "w" str3
        , (bs, "")      <- List.span Char.isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readHex ds, bits)

        -- decimal like 1234w32
        | (ds, str2)    <- List.span Char.isDigit str1
        , not $ null ds
        , Just str3     <- List.stripPrefix "w" str2
        , (bs, "")      <- List.span Char.isDigit str3
        , not $ null bs
        = Just (read ds, read bs)

        | otherwise
        = Nothing


-- | Read a float literal with an explicit format specifier like @123.00f32#@.
readLitFloatOfBits :: String -> Maybe (Double, Maybe Int)
readLitFloatOfBits str1
        | '-' : str2    <- str1
        , Just (d, bs)  <- readLitFloatOfBits str2
        = Just (negate d, bs)

        | (ds1, str2)   <- List.span Char.isDigit str1
        , not $ null ds1
        , Just str3     <- List.stripPrefix "." str2
        , (ds2, str4)   <- List.span Char.isDigit str3
        , not $ null ds2
        = case List.stripPrefix "f" str4 of
                Just str5
                 | (bs, "")     <- List.span Char.isDigit str5
                 , not $ null bs
                 -> Just (read (ds1 ++ "." ++ ds2), Just $ read bs)

                 | otherwise
                 -> Nothing

                Nothing
                 -> Just (read (ds1 ++ "." ++ ds2), Nothing)

        | otherwise
        = Nothing



-- | Read a binary string as a number.
readBinary :: Num a => String -> a
readBinary digits
        = List.foldl' (\acc b -> if b then 2 * acc + 1 else 2 * acc) 0
        $ map (/= '0') digits


-- | Read a hex string as a number.
readHex    :: (Enum a, Num a) => String -> a
readHex digits
        = List.foldl' (\acc d -> let Just v = lookup d table
                                 in  16 * acc + v) 0
        $ digits

 where table
        =  zip ['0' .. '9'] [0  .. 9]
        ++ zip ['a' .. 'f'] [10 .. 15]
        ++ zip ['A' .. 'F'] [10 .. 15]

