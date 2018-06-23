
module DDC.Core.Salt.Name.PrimStore
        ( PrimStore     (..)
        , readPrimStore)
where
import DDC.Data.Pretty
import Control.DeepSeq


-- | Raw access to the store.
data PrimStore
        -- Constants ------------------
        -- | Number of bytes needed to store a value of a primitive type.
        = PrimStoreSize

        -- | Log2 of number of bytes need to store a value of a primitive type.
        | PrimStoreSize2


        -- Addr operations ------------
        -- | Read a value from the store at the given address and offset.
        | PrimStoreRead

        -- | Write a value to the store at the given address and offset.
        | PrimStoreWrite

        -- | Copy a block of memory from the source address to the destination address.
        | PrimStoreCopy

        -- | Set a block of memory to some value.
        | PrimStoreSet

        -- | Add an offset in bytes to an address.
        | PrimStorePlusAddr

        -- | Subtract an offset in bytes from an address.
        | PrimStoreMinusAddr


        -- Ptr operations -------------
        -- | Read a value from a pointer plus offset.
        | PrimStorePeek

        -- | Read a value from a pointer plus offset, with an integrated bounds check.
        | PrimStorePeekBounded

        -- | Write a value to a pointer plus given offset.
        | PrimStorePoke

        -- | Write a value to a pointer plus offset, with an integrated bounds check.
        | PrimStorePokeBounded

        -- | Add an offset in bytes to a pointer.
        | PrimStorePlusPtr

        -- | Subtract an offset in bytes from a pointer.
        | PrimStoreMinusPtr

        -- | Convert an raw address to a pointer.
        | PrimStoreMakePtr

        -- | Convert a pointer to a raw address.
        | PrimStoreTakePtr

        -- | Cast between pointer types.
        | PrimStoreCastPtr


        -- Allocation -----------------
        -- | Check whether there are at least this many bytes still available
        --   on the heap.
        | PrimStoreCheck

        -- | Allocate some space on the heap.
        --   There must be enough space available, else undefined.
        | PrimStoreAlloc

        -- | Allocate space on the stack for a GC root and set it to zero.
        | PrimStoreAllocSlot

        -- | Allocate space on the stack for a GC root and set it to the given value.
        | PrimStoreAllocSlotVal


        -- Global Variables -----------
        -- | Reference to a global
        --   The flag says whether the object level symbol should be initialized in the
        --   code for the enclosing module.
        | PrimStoreGlobal Bool
        deriving (Eq, Ord, Show)


instance NFData PrimStore where
 rnf !_ = ()


instance Pretty PrimStore where
 ppr p
  = case p of
        PrimStoreSize           -> text "size#"
        PrimStoreSize2          -> text "size2#"
        PrimStoreCheck          -> text "check#"
        PrimStoreAlloc          -> text "alloc#"
        PrimStoreAllocSlot      -> text "allocSlot#"
        PrimStoreAllocSlotVal   -> text "allocSlotVal#"

        PrimStoreRead           -> text "read#"
        PrimStoreWrite          -> text "write#"
        PrimStoreCopy           -> text "copy#"
        PrimStoreSet            -> text "set#"
        PrimStorePlusAddr       -> text "plusAddr#"
        PrimStoreMinusAddr      -> text "minusAddr#"

        PrimStorePeek           -> text "peek#"
        PrimStorePoke           -> text "poke#"
        PrimStorePeekBounded    -> text "peekBounded#"
        PrimStorePokeBounded    -> text "pokeBounded#"
        PrimStorePlusPtr        -> text "plusPtr#"
        PrimStoreMinusPtr       -> text "minusPtr#"
        PrimStoreMakePtr        -> text "makePtr#"
        PrimStoreTakePtr        -> text "takePtr#"
        PrimStoreCastPtr        -> text "castPtr#"

        PrimStoreGlobal False   -> text "global#"
        PrimStoreGlobal True    -> text "globali#"



readPrimStore :: String -> Maybe PrimStore
readPrimStore str
 = case str of
        "size#"                 -> Just PrimStoreSize
        "size2#"                -> Just PrimStoreSize2

        "plusAddr#"             -> Just PrimStorePlusAddr
        "minusAddr#"            -> Just PrimStoreMinusAddr
        "read#"                 -> Just PrimStoreRead
        "write#"                -> Just PrimStoreWrite
        "copy#"                 -> Just PrimStoreCopy
        "set#"                  -> Just PrimStoreSet

        "plusPtr#"              -> Just PrimStorePlusPtr
        "minusPtr#"             -> Just PrimStoreMinusPtr
        "makePtr#"              -> Just PrimStoreMakePtr
        "takePtr#"              -> Just PrimStoreTakePtr
        "castPtr#"              -> Just PrimStoreCastPtr
        "peek#"                 -> Just PrimStorePeek
        "poke#"                 -> Just PrimStorePoke
        "peekBounded#"          -> Just PrimStorePeekBounded
        "pokeBounded#"          -> Just PrimStorePokeBounded

        "global#"               -> Just (PrimStoreGlobal False)
        "globali#"              -> Just (PrimStoreGlobal True)

        "check#"                -> Just PrimStoreCheck
        "alloc#"                -> Just PrimStoreAlloc
        "allocSlot#"            -> Just PrimStoreAllocSlot
        "allocSlotVal#"         -> Just PrimStoreAllocSlotVal

        _                       -> Nothing

