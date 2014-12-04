
-- | Reading literal values.
module DDC.Core.Salt.Name.Lit
        ( readLitInteger
        , readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits)

where
import Data.List
import Data.Char


-- | Read a signed integer.
readLitInteger :: String -> Maybe Integer
readLitInteger []       = Nothing
readLitInteger str@(c:cs)
        | '-'           <- c
        , all isDigit cs
        = Just $ read str

        | all isDigit cs
        = Just $ read str
        
        | otherwise
        = Nothing
        

-- | Read an integer with an explicit format specifier like @1234i@.
readLitNat :: String -> Maybe Integer
readLitNat str1
        | (ds, "")      <- span isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an integer literal with an explicit format specifier like @1234i@.
readLitInt :: String -> Maybe Integer
readLitInt str1
        | '-' : str2    <- str1
        , (ds, "i")     <- span isDigit str2
        , not  $ null ds
        = Just $ negate $ read ds

        | (ds, "i")     <- span isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an size literal with an explicit format specifier like @1234s@.
readLitSize :: String -> Maybe Integer
readLitSize str1
        | '-' : str2    <- str1
        , (ds, "s")     <- span isDigit str2
        , not  $ null ds
        = Just $ negate $ read ds

        | (ds, "s")     <- span isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read a word with an explicit format speficier.
readLitWordOfBits :: String -> Maybe (Integer, Int)
readLitWordOfBits str1
        -- binary like 0b01001w32
        | Just str2     <- stripPrefix "0b" str1
        , (ds, str3)    <- span (\c -> c == '0' || c == '1') str2
        , not $ null ds
        , Just str4     <- stripPrefix "w" str3
        , (bs, "")      <- span isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readBinary ds, bits)

        -- hex like 0x0ffw32
        | Just str2     <- stripPrefix "0x" str1
        , (ds, str3)    <- span (\c -> elem c ['0' .. '9']
                                    || elem c ['A' .. 'F']
                                    || elem c ['a' .. 'f']) str2
        , not $ null ds
        , Just str4     <- stripPrefix "w" str3
        , (bs, "")      <- span isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readHex ds, bits)

        -- decimal like 1234w32
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just str3     <- stripPrefix "w" str2
        , (bs, "")      <- span isDigit str3
        , not $ null bs
        = Just (read ds, read bs)

        | otherwise
        = Nothing


-- | Read a float literal with an explicit format specifier like @123.00f32#@.
readLitFloatOfBits :: String -> Maybe (Double, Int)
readLitFloatOfBits str1
        | '-' : str2    <- str1
        , Just (d, bs)  <- readLitFloatOfBits str2
        = Just (negate d, bs)

        | (ds1, str2)   <- span isDigit str1
        , not $ null ds1
        , Just str3     <- stripPrefix "." str2
        , (ds2, str4)   <- span isDigit str3
        , not $ null ds2
        , Just str5     <- stripPrefix "f" str4
        , (bs, "")      <- span isDigit str5
        , not $ null bs
        = Just (read (ds1 ++ "." ++ ds2), read bs)

        | otherwise
        = Nothing


-- | Read a binary string as a number.
readBinary :: Num a => String -> a
readBinary digits
        = foldl' (\acc b -> if b then 2 * acc + 1 else 2 * acc) 0
        $ map (/= '0') digits


-- | Read a hex string as a number.
readHex    :: (Enum a, Num a) => String -> a
readHex digits
        = foldl' (\acc d -> let Just v = lookup d table
                            in  16 * acc + v) 0
        $ digits

 where table
        =  zip ['0' .. '9'] [0  .. 9]
        ++ zip ['a' .. 'f'] [10 .. 15]
        ++ zip ['A' .. 'F'] [10 .. 15]

