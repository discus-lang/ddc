
-- | Names shared by several language profiles.
module DDC.Core.Salt.Name.Prim
        ( PrimTyCon     (..),   readPrimTyCon

        , Prim          (..)
        , PrimCast      (..),   readPrimCast
        , PrimCall      (..),   readPrimCall
        , PrimControl   (..),   readPrimControl
        , PrimStore     (..),   readPrimStore
        , PrimExternal  (..),   readPrimExternal
        , PrimOp        (..),   readPrimOp)
where
import DDC.Base.Pretty
import Data.Char
import Data.List


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | @Void#@ the Void type has no values.
        = PrimTyConVoid

        -- | @Bool#@ unboxed booleans.
        | PrimTyConBool

        -- | @Nat#@ natural numbers.
        --   Big enough to count every addressable byte in the system.
        | PrimTyConNat

        -- | @Int#@ signed integers,
        | PrimTyConInt

        -- | @WordN#@ machine words of the given length.
        | PrimTyConWord   Int

        -- | @FloatN#@ floating point numbers of the given length.
        | PrimTyConFloat  Int

        -- | @Tag#@ data type tags.
        | PrimTyConTag

        -- | @Addr#@ machine addresses.
        | PrimTyConAddr

        -- | @Ptr#@ store pointers.
        | PrimTyConPtr

        -- | @String#@ String of UTF8 characters.
        --   TODO: These are primitive until we can define our own types.
        | PrimTyConString 
        deriving (Eq, Ord, Show)


instance Pretty PrimTyCon where
 ppr tc
  = case tc of
        PrimTyConVoid           -> text "Void#"
        PrimTyConBool           -> text "Bool#"
        PrimTyConNat            -> text "Nat#"
        PrimTyConInt            -> text "Int#"
        PrimTyConWord   bits    -> text "Word"  <> int bits <> text "#"
        PrimTyConFloat  bits    -> text "Float" <> int bits <> text "#"
        PrimTyConTag            -> text "Tag#"
        PrimTyConAddr           -> text "Addr#"
        PrimTyConPtr            -> text "Ptr#"
        PrimTyConString         -> text "String#"


-- | Read a primitive typ constructor.
readPrimTyCon :: String -> Maybe PrimTyCon
readPrimTyCon str
        | str == "Void#"   = Just $ PrimTyConVoid
        | str == "Bool#"   = Just $ PrimTyConBool
        | str == "Nat#"    = Just $ PrimTyConNat
        | str == "Int#"    = Just $ PrimTyConInt
        | str == "Tag#"    = Just $ PrimTyConTag
        | str == "Addr#"   = Just $ PrimTyConAddr
        | str == "Ptr#"    = Just $ PrimTyConPtr
        | str == "String#" = Just $ PrimTyConString

        -- WordN#
        | Just rest     <- stripPrefix "Word" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ PrimTyConWord n

        -- FloatN#
        | Just rest     <- stripPrefix "Float" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [32, 64]
        = Just $ PrimTyConFloat n

        | otherwise
        = Nothing


-- Prim -----------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data    Prim
        -- | Arithmetic and bitwise-operators.
        = PrimOp        PrimOp

        -- | Casting between numeric types.
        | PrimCast      PrimCast

        -- | Funtion calls.
        | PrimCall      PrimCall

        -- | Control flow.
        | PrimControl   PrimControl

        -- | Store access.
        | PrimStore     PrimStore

        -- | External things that should really be imported with an FFI.
        --   We'll remove these when we get the FFI working.
        | PrimExternal  PrimExternal
        deriving (Eq, Ord, Show)


instance Pretty Prim where
 ppr pp
  = case pp of
        PrimOp       op -> ppr op
        PrimCast     c  -> ppr c
        PrimCall     c  -> ppr c
        PrimControl  c  -> ppr c
        PrimStore    p  -> ppr p
        PrimExternal p  -> ppr p


-- PrimCast -------------------------------------------------------------------
-- | Primitive cast between two types.
data PrimCast
        -- | Promote a value to one of a larger or similar size, 
        --   without loss of precision.
        = PrimCastPromote

        -- | Truncate a value to a new width, 
        --   possibly losing precision.
        | PrimCastTruncate
        deriving (Eq, Ord, Show)


instance Pretty PrimCast where
 ppr c
  = case c of
        PrimCastPromote         -> text "promote#"
        PrimCastTruncate        -> text "truncate#"


readPrimCast :: String -> Maybe PrimCast
readPrimCast str
 = case str of
        "promote#"              -> Just PrimCastPromote
        "truncate#"             -> Just PrimCastTruncate
        _                       -> Nothing


-- PrimCall -------------------------------------------------------------------
-- | Primitive ways of invoking a function, 
--   where control flow returns back to the caller.
data PrimCall
        -- | Tailcall a function
        = PrimCallTail    Int
        deriving (Eq, Ord, Show)


instance Pretty PrimCall where
 ppr pc
  = case pc of
        PrimCallTail    arity
         -> text "tailcall" <> int arity <> text "#"


readPrimCall :: String -> Maybe PrimCall
readPrimCall str

        -- tailcallN#
        | Just rest     <- stripPrefix "tailcall" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n > 0
        = Just $ PrimCallTail n

        | otherwise
        = Nothing


-- PrimControl ----------------------------------------------------------------
-- | Primitive non-returning control flow.
data PrimControl
        -- | Ungraceful failure -- just abort the program.
        --   This is called on internal errors in the runtime system.
        --   There is no further debugging info provided, so you'll need to 
        --   look at the stack trace to debug it.
        = PrimControlFail

        -- | Return from the enclosing function with the given value.
        | PrimControlReturn
        deriving (Eq, Ord, Show)


instance Pretty PrimControl where
 ppr pc
  = case pc of
        PrimControlFail         -> text "fail#"
        PrimControlReturn       -> text "return#"


readPrimControl :: String -> Maybe PrimControl
readPrimControl str
 = case str of
        "fail#"         -> Just $ PrimControlFail
        "return#"       -> Just $ PrimControlReturn
        _               -> Nothing


-- PrimStore --------------------------------------------------------------------
-- | A projection of some other object.
data PrimStore
        -- Constants ------------------
        -- | Number of bytes used by a Nat#
        = PrimStoreBytesNat

        -- | Log2 of number of bytes used by a Nat#
        | PrimStoreShiftNat

        -- Allocation -----------------
        -- | Create a heap of the given size.
        --     This must be called before alloc# below, and has global side effect. 
        --     Calling it twice in the same program is undefined.
        | PrimStoreCreate

        -- | Allocate some space on the heap.
        | PrimStoreAlloc

        -- Addr operations ------------
        -- | Read a value from the store at a given address and offset.
        | PrimStoreRead

        -- | Write a value to the store at the given address and offset.
        | PrimStoreWrite

        -- | Add an offset to an address.
        | PrimStorePlusAddr

        -- | Subtract an offset from an address.
        | PrimStoreMinusAddr

        -- Ptr operations -------------
        -- | Read a value from a pointer plus the given offset.
        | PrimStorePeek

        -- | Write a value to a pointer plus the given offset.
        | PrimStorePoke

        -- | Add an offset to a pointer.
        | PrimStorePlusPtr

        -- | Subtract an offset from a pointer.
        | PrimStoreMinusPtr

        -- | Convert an raw address to a pointer.
        | PrimStoreMakePtr

        -- | Convert a pointer to a raw address.
        | PrimStoreTakePtr

        -- | Cast between pointer types.
        | PrimStoreCastPtr
        deriving (Eq, Ord, Show)


instance Pretty PrimStore where
 ppr p
  = case p of        
        PrimStoreBytesNat       -> text "bytesNat#"
        PrimStoreShiftNat       -> text "shiftNat#"

        PrimStoreCreate         -> text "create#"
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
        "bytesNat#"             -> Just PrimStoreBytesNat
        "shiftNat#"             -> Just PrimStoreShiftNat

        "create#"               -> Just PrimStoreCreate
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


-- PrimExternal ---------------------------------------------------------------
-- | String funtions.
--   We're treating these as primops until we get the FFI working.
data PrimExternal
        = PrimExternalShowInt
        | PrimExternalPutStr
        | PrimExternalPutStrLn
        deriving (Eq, Ord, Show)


instance Pretty PrimExternal where
 ppr ps
  = case ps of
        PrimExternalShowInt     -> text "showInt#"
        PrimExternalPutStr      -> text "putStr#"
        PrimExternalPutStrLn    -> text "putStrLn#"


readPrimExternal :: String -> Maybe PrimExternal
readPrimExternal str
        -- showIntN#
        | str == "showInt#"
        = Just $ PrimExternalShowInt

        | str == "putStr#"      
        = Just $ PrimExternalPutStr

        | str == "putStrLn#"    
        = Just $ PrimExternalPutStrLn

        | otherwise
        = Nothing


-- PrimOp ---------------------------------------------------------------------
-- | Primitive numeric, comparison or logic operators.
--   We expect the backend/machine to be able to implement these directly.
data PrimOp
        -- arithmetic
        = PrimOpNeg
        | PrimOpAdd
        | PrimOpSub
        | PrimOpMul
        | PrimOpDiv
        | PrimOpRem

        -- comparison
        | PrimOpEq
        | PrimOpNeq
        | PrimOpGt
        | PrimOpGe
        | PrimOpLt
        | PrimOpLe

        -- boolean
        | PrimOpAnd
        | PrimOpOr

        -- bitwise
        | PrimOpShl
        | PrimOpShr
        | PrimOpBAnd
        | PrimOpBOr
        | PrimOpBXOr
        deriving (Eq, Ord, Show)


instance Pretty PrimOp where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) primOpNames
    in  (text n)


-- | Read a primitive operator.
readPrimOp :: String -> Maybe PrimOp
readPrimOp str
  =  case find (\(_, n) -> str == n) primOpNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
primOpNames :: [(PrimOp, String)]
primOpNames
 =      [ (PrimOpNeg,           "neg#")
        , (PrimOpAdd,           "add#")
        , (PrimOpSub,           "sub#")
        , (PrimOpMul,           "mul#")
        , (PrimOpDiv,           "div#")
        , (PrimOpRem,           "rem#")
        , (PrimOpEq ,           "eq#" )
        , (PrimOpNeq,           "neq#")
        , (PrimOpGt ,           "gt#" )
        , (PrimOpGe ,           "ge#" )
        , (PrimOpLt ,           "lt#" )
        , (PrimOpLe ,           "le#" )
        , (PrimOpAnd,           "and#")
        , (PrimOpOr ,           "or#" ) 
        , (PrimOpShl,           "shl#")
        , (PrimOpShr,           "shr#")
        , (PrimOpBAnd,          "band#")
        , (PrimOpBOr,           "bor#")
        , (PrimOpBXOr,          "bxor#") ]

