
module DDC.Core.Llvm.Platform
        ( Platform(..)
        , defaultPlatform)
where


-- | Enough information about the platform to generate LLVM code for it.
data Platform
        = Platform
        { -- Width of an address on this platform, in bytes.
          platformAddrBytes             :: Int 

          -- Width of a constructor tag, in bytes.
        , platformTagBytes              :: Int 

          -- Width of the object header word, in bytes.
        , platformHeaderBytes           :: Int 

          -- Align functions on this boundary, in bytes
        , platformFunctionAlignBytes    :: Int }


-- | Default platform setup with the given word size, in bytes.
--
--   Addresses have the given width, but we treat object header words
--   as 32-bit regardless.
defaultPlatform :: Int -> Platform
defaultPlatform bytes
        = Platform
        { platformAddrBytes             = bytes
        , platformTagBytes              = 4
        , platformHeaderBytes           = 4
        , platformFunctionAlignBytes    = bytes }


