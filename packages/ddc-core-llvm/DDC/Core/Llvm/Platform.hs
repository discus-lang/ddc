
module DDC.Core.Llvm.Platform
        ( Platform      (..)
        , platform32
        , platform64)
where


-- | Enough information about the platform to generate LLVM code for it.
data Platform
        = Platform
        { -- Width of an address.
          platformAddrBytes     :: Integer

          -- Width of a constructor tag.
        , platformTagBytes      :: Integer

          -- Width of a Nat (used for object sizes like size_t).
        , platformNatBytes      :: Integer

          -- Align functions on this boundary.
        , platformAlignBytes    :: Integer 

          -- Minimum size of a heap object.
        , platformObjBytes      :: Integer }
        deriving Show


-- | 32-bit platform.
--   Heap objects are aligned to 64-bit so that double-precision floats
--   in the object payloads maintain their alignments.
platform32 :: Platform
platform32
        = Platform
        { platformAddrBytes     = 4
        , platformTagBytes      = 4
        , platformNatBytes      = 4
        , platformAlignBytes    = 4 
        , platformObjBytes      = 8 }


-- | 64-bit platform.
platform64 :: Platform
platform64 
        = Platform
        { platformAddrBytes     = 8
        , platformTagBytes      = 4
        , platformNatBytes      = 8
        , platformAlignBytes    = 8
        , platformObjBytes      = 8 }
