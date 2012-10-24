
module DDC.Core.Salt.Platform
        ( Platform      (..)
        , platform32
        , platform64)
where
import DDC.Base.Pretty

-- | Enough information about the platform to generate code for it.
--   We need to know the pointer size, and alignment constraints
--   so that we can lay out heap objects.
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

instance Pretty Platform where
 ppr pp
  = vcat
        [ text "Address Width       (bytes) : "
                <> text (show $ platformAddrBytes  pp)

        , text "Tag Word Width      (bytes) : " 
                <> text (show $ platformTagBytes   pp)

        , text "Nat Word Width      (bytes) : " 
                <> text (show $ platformNatBytes   pp)

        , text "Function Alignment  (bytes) : " 
                <> text (show $ platformAlignBytes pp)

        , text "Minimum Object Size (bytes) : " 
                <> text (show $ platformObjBytes   pp) ]


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
