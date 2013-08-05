
-- | Reading literal values.
module DDC.Core.Salt.Name.Lit
        ( readLitInteger
        , readLitPrimNat
        , readLitPrimInt
        , readLitPrimWordOfBits
        , readLitPrimFloatOfBits)

where
import Data.List
import Data.Char


-- | Read a signed integer.
readLitInteger :: String -> Maybe Integer
readLitInteger []       = Nothing
readLitInteger str@(c:cs)
        | '-'   <- c
        , all isDigit cs
        = Just $ read str

        | all isDigit str
        = Just $ read str
        
        | otherwise
        = Nothing
        

-- | Read an integer with an explicit format specifier like @1234i#@.
readLitPrimNat :: String -> Maybe Integer
readLitPrimNat str1
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just ""       <- stripPrefix "#" str2
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an integer literal with an explicit format specifier like @1234i#@.
readLitPrimInt :: String -> Maybe Integer
readLitPrimInt str1
        | '-' : str2    <- str1
        , (ds, "i#")    <- span isDigit str2
        , not $ null ds
        = Just $ read ds

        | (ds, "i#")    <- span isDigit str1
        , not $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read a word with an explicit format speficier.
readLitPrimWordOfBits :: String -> Maybe (Integer, Int)
readLitPrimWordOfBits str1
        -- binary like 0b01001w32#
        | Just str2     <- stripPrefix "0b" str1
        , (ds, str3)    <- span (\c -> c == '0' || c == '1') str2
        , not $ null ds
        , Just str4     <- stripPrefix "w" str3
        , (bs, "#")     <- span isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readBinary ds, bits)

        -- decimal like 1234w32#
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just str3     <- stripPrefix "w" str2
        , (bs, "#")     <- span isDigit str3
        , not $ null bs
        = Just (read ds, read bs)

        | otherwise
        = Nothing


-- | Read a float literal with an explicit format specifier like @123.00f32#@.
readLitPrimFloatOfBits :: String -> Maybe (Double, Int)
readLitPrimFloatOfBits str1
        | '-' : str2    <- str1
        , Just (d, bs)  <- readLitPrimFloatOfBits str2
        = Just (negate d, bs)

        | (ds1, str2)   <- span isDigit str1
        , not $ null ds1
        , Just str3     <- stripPrefix "." str2
        , (ds2, str4)   <- span isDigit str3
        , not $ null ds2
        , Just str5     <- stripPrefix "f" str4
        , (bs, "#")     <- span isDigit str5
        , not $ null bs
        = Just (read (ds1 ++ "." ++ ds2), read bs)

        | otherwise
        = Nothing


-- | Read a binary string as a number.
readBinary :: (Num a, Read a) => String -> a
readBinary digits
        = foldl' (\ acc b -> if b then 2 * acc + 1 else 2 * acc) 0
        $ map (/= '0') digits

