
module DDC.Source.Tetra.Lexer.Lit
        ( readLitInteger
        , readLitNat
        , readLitInt
        , readLitWordOfBits)
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
        

-- | Read a natural number like @1234@.
readLitNat :: String -> Maybe Integer
readLitNat str1
        | (ds, "")      <- span isDigit str1
        , not  $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read an integer with an explicit format specifier like @1234i@.
readLitInt :: String -> Maybe Integer
readLitInt str1
        | '-' : str2    <- str1
        , (ds, "i")     <- span isDigit str2
        , not $ null ds
        = Just $ read ds

        | (ds, "i")     <- span isDigit str1
        , not $ null ds
        = Just $ read ds

        | otherwise
        = Nothing


-- | Read a word with an explicit format speficier.
readLitWordOfBits :: String -> Maybe (Integer, Int)
readLitWordOfBits str1
        -- binary like 0b01001w32#
        | Just str2     <- stripPrefix "0b" str1
        , (ds, str3)    <- span (\c -> c == '0' || c == '1') str2
        , not $ null ds
        , Just str4     <- stripPrefix "w" str3
        , (bs, "")      <- span isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readBinary ds, bits)

        -- decimal like 1234w32#
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just str3     <- stripPrefix "w" str2
        , (bs, "")      <- span isDigit str3
        , not $ null bs
        = Just (read ds, read bs)

        | otherwise
        = Nothing


-- | Read a binary string as a number.
readBinary :: (Num a, Read a) => String -> a
readBinary digits
        = foldl' (\ acc b -> if b then 2 * acc + 1 else 2 * acc) 0
        $ map (/= '0') digits

