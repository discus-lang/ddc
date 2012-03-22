
module DDC.Core.Llvm.Runtime.Alloc
        ( allocBytes
        , allocDataRaw
        , allocDataRawSmall)
where
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Exp
import DDC.Llvm.Instr
import Control.Monad.State.Strict
import Data.Sequence            (Seq, (<|))
import qualified Data.Sequence  as Seq


-- Alloc ----------------------------------------------------------------------
-- | Allocate some bytes on the heap.
allocBytes :: Var -> Integer -> LlvmM (Seq Instr)
allocBytes vObj bytes
 = do   platform        <- gets llvmStatePlatform
        Var nMalloc _   <- getPrimVarM "malloc"
        let bytes'      = roundUpBytes (platformHeapAlignBytes platform) bytes
        let tResult     = typeOfVar vObj
        return  $ Seq.fromList
                [ IComment ["allocBytes " ++ show bytes]
                , ICall (Just vObj) CallTypeStd tResult nMalloc [XLit $ litNat platform bytes'] []]


-- | Allocate a DataRaw object.
allocDataRaw
        :: Var          -- ^ Var for object.
        -> Integer      -- ^ Tag of object.
        -> Integer      -- ^ Playload size in bytes.
        -> LlvmM (Seq Instr)

allocDataRaw vObj _tag bytesPayload
 = do   struct          <- getStructM "DataRaw"
        bytesStruct     <- getBytesOfStructM struct
        ssAlloc         <- allocBytes vObj $ bytesStruct + bytesPayload

        return  $  IComment ["allocDataRaw"]
                <| ssAlloc


-- | Allocate a DataRawSmall object.
allocDataRawSmall 
        :: Var          -- ^ Var for object.
        -> Integer      -- ^ Tag of object.
        -> Integer      -- ^ Payload size in bytes.
        -> LlvmM (Seq Instr)

allocDataRawSmall vObj _tag bytesPayload
 = do   struct          <- getStructM "DataRawSmall"
        bytesStruct     <- getBytesOfStructM struct
        ssAlloc         <- allocBytes vObj $ bytesStruct + bytesPayload

        return  $  IComment ["allocDataRawSmall"]
                <| ssAlloc


-- Utils ----------------------------------------------------------------------
-- | Round up to a multiple of the given number.
roundUpBytes :: Integer -> Integer -> Integer
roundUpBytes align n
        | n <= 0
        = error $ "roundUpBytes: allocate with " ++ show n

        | mod n align == 0
        = n

        | otherwise
        = n + (align - mod n align)


litNat :: Platform -> Integer -> Lit
litNat platform i
        = LitInt (TInt (8 * platformAddrBytes platform))
                 (fromIntegral i) 



