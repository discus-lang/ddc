
module DDC.Core.Llvm.Platform
        ( Platform      (..)
        , Struct        (..)
        , Field         (..)
        , platform32)
where
import DDC.Llvm.Type
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- | Enough information about the platform to generate LLVM code for it.
data Platform
        = Platform
        { -- Width of an address on this platform, in bytes.
          platformAddrBytes             :: Integer

          -- Width of a constructor tag, in bytes.
        , platformTagBytes              :: Integer

          -- Width of the object header word, in bytes.
        , platformHeaderBytes           :: Integer

          -- Align heap objects on this boundary, in bytes.
        , platformHeapAlignBytes        :: Integer

          -- Align functions on this boundary, in bytes.
        , platformFunctionAlignBytes    :: Integer

          -- Structures used by the runtime system.
        , platformStructs               :: Map String Struct }


-- Structs --------------------------------------------------------------------
-- | Defines a structure used by the runtime system.
data Struct
        = Struct
        { structName            :: String
        , structFields          :: [Field] }

-- | A struct field.
data Field
        = Field
        { structFieldName       :: String
        , structFieldType       :: Type }

        | Pad
        { structFieldPadBytes   :: Integer }


-- | Convert a list of structs to a map of them.
makeStructMap :: [Struct] -> Map String Struct
makeStructMap structs
 = Map.fromList
        [ (name, struct) | struct@(Struct name _) <- structs ]


-- 32-bit platform  -----------------------------------------------------------
-- | 32-bit platform.
--   Heap objects are aligned to 64-bit so that double-precision floats
--   in the object payloads maintain their alignments.
platform32 :: Platform
platform32
        = Platform
        { platformAddrBytes             = 4
        , platformTagBytes              = 4
        , platformHeaderBytes           = 4
        , platformHeapAlignBytes        = 8
        , platformFunctionAlignBytes    = 4
        , platformStructs               = makeStructMap structs32 }        


structs32 :: [Struct]
structs32
 =      [ Struct "Obj"
                 [ Field "header"  (TInt 32) ]

        , Struct "DataBoxed"
                 [ Field "header"  (TInt 32) 
                 , Field "arity"   (TInt 32)
                 , Field "payload" (TArray 0 (TPointer tObj32)) ]

        , Struct "DataMixed"
                 [ Field "header"  (TInt 32)
                 , Pad 4
                 , Field "size"    (TInt 32)
                 , Field "ptrs"    (TInt 32) 
                 , Field "payload" (TArray 0 (TPointer tObj32)) ]

        , Struct "DataRaw"
                 [ Field "header"  (TInt 32)
                 , Field "size"    (TInt 32)
                 , Field "payload" (TArray 0 (TInt 32)) ]

        , Struct "DataRawSmall"
                 [ Field "header"  (TInt 32)
                 , Field "payload" (TArray 0 (TInt 32)) ]
        ]

tObj32 :: Type
tObj32  = TStruct [TInt 32]

