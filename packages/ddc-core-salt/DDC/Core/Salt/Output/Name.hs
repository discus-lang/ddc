
-- | Names used in the Disciple Core Salt language profile.
--   These map directly onto names used in the C output language.
module DDC.Core.Salt.Output.Name
        ( Name            (..)
        , Prim            (..)
        , PrimTyCon       (..)
        , PrimOp          (..)
        , PrimCast        (..)
        , PrimCall        (..)
        , PrimControl     (..)
        , PrimStore       (..)
        , PrimExternal    (..)
        , readName)
where
import DDC.Core.Salt.Base.Name
import DDC.Base.Pretty
import Data.Char
import Data.List


-- | Names of things used in Disciple Core Salt.
data Name
        -- | A type or value variable
        = NameVar       String

        -- | We still need a constructor-like name for modules.
        | NameCon       String

        -- | The object type constructor.
        | NameObjTyCon

        -- | A type primitive constructor.
        | NamePrimTyCon PrimTyCon

        -- | A primitive operator.
        | NamePrim      Prim

        -- | The void literal.
        | NameVoid

        -- | A natural number literal.
        | NameNat       Integer

        -- | A constructor tag literal.
        | NameTag       Integer

        -- | A boolean literal.
        | NameBool      Bool

        -- | A WordN literal, of the given width.
        | NameWord      Integer Int

        -- | An IntN literal, of the given width.
        | NameInt       Integer Int
        deriving (Eq, Ord, Show)


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n        -> text n
        NameCon  n        -> text n
        NameObjTyCon      -> text "Obj"
        NamePrimTyCon tc  -> ppr tc
        NamePrim p        -> ppr p
        NameVoid          -> text "V#"
        NameNat  i        -> integer i
        NameTag  i        -> text "TAG" <> integer i <> text "#"
        NameBool True     -> text "True#"
        NameBool False    -> text "False#"
        NameWord i bits   -> integer i <> text "w" <> int bits <> text "#"
        NameInt  i bits   -> integer i <> text "i" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Obj 
        | str == "Obj"
        = Just $ NameObjTyCon

        -- PrimTyCon
        | Just p        <- readPrimTyCon str
        = Just $ NamePrimTyCon p

        -- PrimOp
        | Just p        <- readPrimOp str
        = Just $ NamePrim $ PrimOp p

        -- PrimCast
        | Just p        <- readPrimCast str
        = Just $ NamePrim $ PrimCast p

        -- PrimCall
        | Just p        <- readPrimCall str
        = Just $ NamePrim $ PrimCall p

        -- PrimControl
        | Just p        <- readPrimControl str
        = Just $ NamePrim $ PrimControl p

        -- PrimStore
        | Just p        <- readPrimStore str
        = Just $ NamePrim $ PrimStore p

        -- PrimExternal
        | Just p        <- readPrimExternal str
        = Just $ NamePrim $ PrimExternal p

        -- Literal void
        | str == "V#" = Just $ NameVoid

        -- Literal Nats.
        | Just i        <- readLitInteger str
        = Just $ NameNat i     

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ NameTag (read ds)

        -- Literal Bools
        | str == "True#"  = Just $ NameBool True
        | str == "False#" = Just $ NameBool False

        -- Literal Words
        | Just (val, bits) <- readLitPrimWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameWord val bits

        -- Literal Ints
        | Just (val, bits) <- readLitPrimIntOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameInt  val bits

        -- Constructors.
        | c : _         <- str
        , isUpper c      
        = Just $ NameVar str

        -- Variables.
        | c : _         <- str
        , isLower c      
        = Just $ NameVar str

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


-- Store -----------------------------------------------------------------------
-- | A projection of some other object.
data PrimStore
        -- | Allocate some space on the heap.
        = PrimStoreAlloc

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
        = PrimExternalShowInt Int
        | PrimExternalPutStr
        | PrimExternalPutStrLn
        deriving (Eq, Ord, Show)


instance Pretty PrimExternal where
 ppr ps
  = case ps of
        PrimExternalShowInt i   -> text "showInt" <> int i <> text "#"
        PrimExternalPutStr      -> text "putStr#"
        PrimExternalPutStrLn    -> text "putStrLn#"


readPrimExternal :: String -> Maybe PrimExternal
readPrimExternal str
        -- showIntN#
        | Just rest     <- stripPrefix "showInt" str
        , (ds, "#")     <- span isDigit rest
        , bits          <- read ds
        , elem bits [8, 16, 32, 64]
        = Just $ PrimExternalShowInt bits

        | str == "putStr#"      
        = Just $ PrimExternalPutStr

        | str == "putStrLn#"    
        = Just $ PrimExternalPutStrLn

        | otherwise
        = Nothing

