
module DDC.Core.Codec.Shimmer.Hash
        ( Config (..)
        , hashExpAsByteString
        , hashExpAsWord64s)
where
import DDC.Core.Codec.Shimmer.Encode
import Data.Word
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString        as BS
import qualified DDC.Core.Exp           as C
import qualified SMR.Core.Codec         as S
import qualified Foreign.Ptr            as Foreign
import qualified Foreign.Storable       as Foreign
import qualified System.IO.Unsafe       as System
import qualified "cryptohash-sha256" Crypto.Hash.SHA256  as Sha256


-- | Compute the Sha256 hash of a core expression,
--   producing the hash value as a bytestring.
hashExpAsByteString :: Config n -> C.Exp a n -> BS.ByteString
hashExpAsByteString c xx
 = let  sExp  = takeExp c xx
        bsExp = S.packExp sExp
   in   Sha256.hash bsExp


-- | Compute the Sha256 hash of a core expression,
--   producing the hash value as separate Word64s.
hashExpAsWord64s
        :: Config n -> C.Exp a n
        -> (Word64, Word64, Word64, Word64)
hashExpAsWord64s c xx
 = System.unsafePerformIO
 $ do   let bsHash  = hashExpAsByteString c xx
        BS.unsafeUseAsCString bsHash $ \pCString
         -> do  let pWord64 = Foreign.castPtr pCString
                w0 <- Foreign.peekElemOff pWord64 0
                w1 <- Foreign.peekElemOff pWord64 1
                w2 <- Foreign.peekElemOff pWord64 2
                w3 <- Foreign.peekElemOff pWord64 3
                return  (w0, w1, w2, w3)


