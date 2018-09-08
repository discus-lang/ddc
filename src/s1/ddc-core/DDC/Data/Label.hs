{-# LANGUAGE PackageImports #-}
module DDC.Data.Label
        ( Label
        , labelOfText
        , hashOfLabel, nameOfLabel)
where
import Control.DeepSeq
import Data.Bits
import qualified "cryptohash-sha256" Crypto.Hash.SHA256     as Sha256
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified GHC.Word               as W


-- | Label type.
data Label
        = Label
        { labelHash     :: !W.Word64
        , labelName     :: !T.Text }
        deriving Show


instance NFData Label where
 rnf _ = ()


instance Eq Label where
 (==) (Label h1 _) (Label h2 _) = h1 == h2
 {-# INLINE (==) #-}

 (/=) (Label h1 _) (Label h2 _) = h1 /= h2
 {-# INLINE (/=) #-}


instance Ord Label where
 compare (Label h1 _) (Label h2 _) = compare h1 h2
 {-# INLINE compare #-}


-- | Construct a label from a text string.
labelOfText :: T.Text -> Label
labelOfText tx
        = Label
        { labelHash     = shortHashOfText tx
        , labelName     = tx }


-- | Get the hash code of a label.
hashOfLabel :: Label -> W.Word64
hashOfLabel (Label h _) = h
{-# INLINE hashOfLabel #-}


-- | Get the name of a label.
nameOfLabel :: Label -> T.Text
nameOfLabel (Label _ n) = n
{-# INLINE nameOfLabel #-}


-- | Get a short hash code for a text string.
--   We use this as a (most likely) unique key for names.
shortHashOfText :: T.Text -> W.Word64
shortHashOfText tx
 = let  bs      = Sha256.hash $ T.encodeUtf8 tx
        (w0 : w1 : w2 : w3 : w4 : w5 : w6 : w7 : _)
                = BS.unpack bs

   in   (shift (fromIntegral w0) 56)
    .|. (shift (fromIntegral w1) 48)
    .|. (shift (fromIntegral w2) 40)
    .|. (shift (fromIntegral w3) 32)
    .|. (shift (fromIntegral w4) 24)
    .|. (shift (fromIntegral w5) 16)
    .|. (shift (fromIntegral w6)  8)
    .|.        (fromIntegral w7)

