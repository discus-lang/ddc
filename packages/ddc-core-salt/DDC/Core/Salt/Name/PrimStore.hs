
module DDC.Core.Salt.Name.PrimStore
        ( PrimStore     (..)
        , readPrimStore)
where
import DDC.Base.Pretty
import Control.DeepSeq


-- | Raw access to the store.
data PrimStore
        -- Constants ------------------
        -- | Number of bytes needed to store a value of a primitive type.
        = PrimStoreSize

        -- | Log2 of number of bytes need to store a value of a primitive type.
        | PrimStoreSize2

        -- Allocation -----------------
        -- | Create a heap of the given size.
        --     This must be called before @alloc#@ below, and has global side effect. 
        --     Calling it twice in the same program is undefined.
        | PrimStoreCreate

        -- | Check whether there are at least this many bytes still available
        --   on the heap.
        | PrimStoreCheck

        -- | Force a garbage collection to recover at least this many bytes.
        | PrimStoreRecover

        -- | Allocate some space on the heap.
        --   There must be enough space available, else undefined.
        | PrimStoreAlloc

        -- Addr operations ------------
        -- | Read a value from the store at the given address and offset.
        | PrimStoreRead

        -- | Write a value to the store at the given address and offset.
        | PrimStoreWrite

        -- | Add an offset in bytes to an address.
        | PrimStorePlusAddr

        -- | Subtract an offset in bytes from an address.
        | PrimStoreMinusAddr

        -- Ptr operations -------------
        -- | Read a value from a pointer plus the given offset.
        | PrimStorePeek

        -- | Write a value to a pointer plus the given offset.
        | PrimStorePoke

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
        deriving (Eq, Ord, Show)


instance NFData PrimStore


instance Pretty PrimStore where
 ppr p
  = case p of        
        PrimStoreSize           -> text "size#"
        PrimStoreSize2          -> text "size2#"        
        PrimStoreCreate         -> text "create#"
        PrimStoreCheck          -> text "check#"
        PrimStoreRecover        -> text "recover#"
        PrimStoreAlloc          -> text "alloc#"

        PrimStoreRead           -> text "read#"
        PrimStoreWrite          -> text "write#"
        PrimStorePlusAddr       -> text "plusAddr#"
        PrimStoreMinusAddr      -> text "minusAddr#"

        PrimStorePeek           -> text "peek#"
        PrimStorePoke           -> text "poke#"
        PrimStorePlusPtr        -> text "plusPtr#"
        PrimStoreMinusPtr       -> text "minusPtr#"
        PrimStoreMakePtr        -> text "makePtr#"
        PrimStoreTakePtr        -> text "takePtr#"
        PrimStoreCastPtr        -> text "castPtr#"


readPrimStore :: String -> Maybe PrimStore
readPrimStore str
 = case str of
        "size#"                 -> Just PrimStoreSize
        "size2#"                -> Just PrimStoreSize2

        "create#"               -> Just PrimStoreCreate
        "check#"                -> Just PrimStoreCheck
        "recover#"              -> Just PrimStoreRecover
        "alloc#"                -> Just PrimStoreAlloc

        "read#"                 -> Just PrimStoreRead
        "write#"                -> Just PrimStoreWrite
        "plusAddr#"             -> Just PrimStorePlusAddr
        "minusAddr#"            -> Just PrimStoreMinusAddr

        "peek#"                 -> Just PrimStorePeek
        "poke#"                 -> Just PrimStorePoke
        "plusPtr#"              -> Just PrimStorePlusPtr
        "minusPtr#"             -> Just PrimStoreMinusPtr
        "makePtr#"              -> Just PrimStoreMakePtr
        "takePtr#"              -> Just PrimStoreTakePtr
        "castPtr#"              -> Just PrimStoreCastPtr

        _                       -> Nothing

