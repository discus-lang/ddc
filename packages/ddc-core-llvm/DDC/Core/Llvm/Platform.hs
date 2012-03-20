
module DDC.Core.Llvm.Platform
        ( Platform(..)
        , defaultPlatform)
where


-- | Enough information about the platform to generate LLVM code for it.
data Platform
        = Platform
        { -- Width of an address on this platform in bits.
          platformAddrWidth     :: Int 

          -- Width of a constructor tag in bits.
        , platformTagWidth      :: Int 

          -- Width of the object header word in bits.
        , platformHeaderWidth   :: Int 

          -- Align functions on this boundary, in bytes
        , platformAlignFunctions :: Int }


-- | Default platform setup with the given word size, in bytes.
--
--   Addresses have the given width, but we treat object header words
--   as 32-bit regardless.
defaultPlatform :: Int -> Platform
defaultPlatform bytes
        = Platform
        { platformAddrWidth             = bytes * 8
        , platformTagWidth              = 32
        , platformHeaderWidth           = 32
        , platformAlignFunctions        = bytes }


