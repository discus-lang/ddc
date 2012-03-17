
-- | Names used in the SeaOutput language profile.
--   These map directly onto names used in the C output language.
module DDC.Core.Sea.Output.Name
        ( Name          (..)
        , Prim          (..)
        , PrimTyCon     (..)
        , PrimOp        (..)
        , PrimProj      (..)
        , PrimCast      (..)
        , PrimAlloc     (..)
        , PrimCall      (..)
        , PrimControl   (..)
        , PrimString    (..)
        , PrimIO        (..)
        , readName)
where
import DDC.Core.Sea.Base.Name   (PrimTyCon(..), PrimOp(..))
import DDC.Base.Pretty
import Data.Char
import Data.List

-- Names of things recognised by the Sea backend.
data Name
        -- | The object type constructor.
        = NameObjTyCon

        -- | A type primitive constructor.
        | NamePrimTyCon PrimTyCon

        -- | A type or value variable
        | NameVar       String

        -- | A primitive operator.
        | NamePrim      Prim

        -- | A natural number literal
        | NameNat       Integer
        deriving (Eq, Ord, Show)


instance Pretty Name where
 ppr nn
  = case nn of
        NameObjTyCon      -> text "Obj"
        NamePrimTyCon tc  -> ppr tc
        NameVar  n        -> text n
        NamePrim p        -> ppr p
        NameNat  i        -> text (show i)


-- Prim -----------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data    Prim
        -- | Invoke a primitive arithmetic operator.
        = PrimOp        PrimOp

        -- | Project something from an object.
        | PrimProj      PrimProj

        -- | Casting between numeric types.
        | PrimCast      PrimCast

        -- | Allocate an object.
        | PrimAlloc     PrimAlloc

        -- | Calling functions, internal or external.
        | PrimCall      PrimCall

        -- | Control flow.
        | PrimControl   PrimControl

        -- | Strings.
        | PrimString    PrimString

        -- | IO.
        | PrimIO        PrimIO
        deriving (Eq, Ord, Show)


instance Pretty Prim where
 ppr pp
  = case pp of
        PrimOp op       -> ppr op
        PrimProj j      -> ppr j
        PrimCast c      -> ppr c
        PrimCall c      -> ppr c
        PrimAlloc a     -> ppr a
        PrimControl c   -> ppr c
        PrimString s    -> ppr s
        PrimIO i        -> ppr i


-- Proj -----------------------------------------------------------------------
-- | A projection of some other object.
data PrimProj
        -- | Take the tag of a boxed object.
        = PrimProjTag

        -- | Take a numbered field from some boxed data object.
        | PrimProjField
        deriving (Eq, Ord, Show)


instance Pretty PrimProj where
 ppr j
  = case j of
        PrimProjTag     -> text "tag#"
        PrimProjField   -> text "field#"


-- PrimCast -------------------------------------------------------------------
-- | Primitive cast between two types.
data PrimCast
        = PrimCastOp
        | PrimCastNatToInt Int
        deriving (Eq, Ord, Show)


instance Pretty PrimCast where
 ppr c
  = case c of
        PrimCastOp      -> text "cast#"

        PrimCastNatToInt bits
         -> text "i" <> int bits <> text "#"


-- PrimAlloc ------------------------------------------------------------------
-- | Allocation of objects.
data PrimAlloc
        -- | Allocate a suspended or partial application,
        --   and fill in the function pointer, function arity, 
        --   and number of args in the thunk.
        = PrimAllocThunk

        -- | Allocate a fresh DataBoxed object,
        --   and fill in the constructor tag and arity.
        | PrimAllocBoxed

        -- | Allocate a fresh DataRaw object
        --   and fill in the constructor tag, and raw payload size.
        | PrimAllocRaw

        -- | Allocate a fresh DataMixed object,
        --   and fill in the constructor tag, 
        --     number of boxed objects,
        --     and the raw payload size.
        | PrimAllocMixed
        deriving (Eq, Ord, Show)


instance Pretty PrimAlloc where
 ppr pa
  = case pa of
        PrimAllocThunk          -> text "thunk#"
        PrimAllocBoxed          -> text "boxed#"
        PrimAllocRaw            -> text "raw#"
        PrimAllocMixed          -> text "mixed#"


-- PrimCall -------------------------------------------------------------------
-- | Primitive ways of invoking a function.
data PrimCall
        -- | Tailcall a top level function..
        = PrimCallTail    Int

        -- | Build a partial application.
        | PrimCallPartial Int Int

        -- | Apply a partial application.
        | PrimCallApply   Int

        -- | Force a suspended application
        | PrimCallForce
        deriving (Eq, Ord, Show)


instance Pretty PrimCall where
 ppr pc
  = case pc of
        PrimCallTail    arity
         -> text "tailcall" <> int arity <> text "#"

        PrimCallPartial arity args 
         -> text "partial"  <> int arity <> text "of" <> int args <> text "#"

        PrimCallApply   args
         -> text "apply"    <> int args  <> text "#"

        PrimCallForce
         -> text "force#"


-- PrimControl ----------------------------------------------------------------
-- | Primitive control flow.
data PrimControl
        -- | Return from the enclosing function with the given value.
        = PrimControlReturn
        deriving (Eq, Ord, Show)

instance Pretty PrimControl where
 ppr pc
  = case pc of
        PrimControlReturn 
         -> text "return#"


-- PrimString -----------------------------------------------------------------
-- | String funtions.
--   We're treating these as primops until we get the FFI working.
data PrimString 
        = PrimStringShowInt Int
        deriving (Eq, Ord, Show)

instance Pretty PrimString where
 ppr ps
  = case ps of
        PrimStringShowInt i
         -> text "showInt" <> int i <> text "#"


-- PrimIO ---------------------------------------------------------------------
-- | IO functions.
--   We're treating these as primops until we get the FFI working.
data PrimIO
        = PrimIOPutStr
        deriving (Eq, Ord, Show)

instance Pretty PrimIO where
 ppr ps
  = case ps of
        PrimIOPutStr
         -> text "putStr#"


-- Parsing --------------------------------------------------------------------
readName :: String -> Maybe Name
readName []     = Nothing
readName str@(c:_)
        -- Primitive tycons
        | str == "Void#"   = Just $ NamePrimTyCon PrimTyConVoid
        | str == "Ptr#"    = Just $ NamePrimTyCon PrimTyConPtr
        | str == "Addr#"   = Just $ NamePrimTyCon PrimTyConAddr
        | str == "Nat#"    = Just $ NamePrimTyCon PrimTyConNat
        | str == "Tag#"    = Just $ NamePrimTyCon PrimTyConTag
        | str == "Bool#"   = Just $ NamePrimTyCon PrimTyConBool
        | str == "String#" = Just $ NamePrimTyCon PrimTyConString

        -- IntN#
        | Just rest     <- stripPrefix "Int" str
        , (ds, "#")     <- span isDigit rest
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ NamePrimTyCon (PrimTyConInt n)

        -- Obj
        | str == "Obj"
        = Just $ NameObjTyCon

        -- Sea Primops ------------------------------------
        | str == "tag#"         = Just $ NamePrim $ PrimProj    PrimProjTag
        | str == "field#"       = Just $ NamePrim $ PrimProj    PrimProjField
        | str == "cast#"        = Just $ NamePrim $ PrimCast    PrimCastOp
        | str == "force#"       = Just $ NamePrim $ PrimCall    PrimCallForce
        | str == "thunk#"       = Just $ NamePrim $ PrimAlloc   PrimAllocThunk
        | str == "boxed#"       = Just $ NamePrim $ PrimAlloc   PrimAllocBoxed
        | str == "raw#"         = Just $ NamePrim $ PrimAlloc   PrimAllocRaw
        | str == "mixed#"       = Just $ NamePrim $ PrimAlloc   PrimAllocMixed
        | str == "return#"      = Just $ NamePrim $ PrimControl PrimControlReturn

        -- tailcallN#
        | Just rest     <- stripPrefix "tailcall" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n > 0
        = Just $ NamePrim $ PrimCall (PrimCallTail n)

        -- partialNofM#
        | Just  rest    <- stripPrefix "partial" str
        , (dsn, rest2)  <- span isDigit rest
        , Just  rest3   <- stripPrefix "of" rest2
        , (dsm, "#")    <- span isDigit rest3
        , not $ null dsn
        , n             <- read dsn
        , n > 0
        , not $ null dsm
        , m             <- read dsm
        , m > 0
        , n < m
        = Just $ NamePrim $ PrimCall (PrimCallPartial n m)

        -- applyN#
        | Just  rest    <- stripPrefix "apply" str
        , (dsn, "#")    <- span isDigit rest
        , not $ null dsn
        , n             <- read dsn
        , n > 0
        = Just $ NamePrim $ PrimCall (PrimCallApply n)

        -- Casts ------------------------------------------
        -- Cast Nat to Int
        | Just rest     <- stripPrefix "i" str
        , (ds, "#")     <- span isDigit rest
        , bits          <- read ds
        , elem bits [8, 16, 32, 64]
        = Just $ NamePrim $ PrimCast $ PrimCastNatToInt bits

        -- Arithmetic Primops -----------------------------
        | str == "add#"         = Just $ NamePrim $ PrimOp PrimOpAdd
        | str == "sub#"         = Just $ NamePrim $ PrimOp PrimOpSub
        | str == "mul#"         = Just $ NamePrim $ PrimOp PrimOpMul
        | str == "div#"         = Just $ NamePrim $ PrimOp PrimOpDiv
        | str == "mod#"         = Just $ NamePrim $ PrimOp PrimOpMod
        | str == "eq#"          = Just $ NamePrim $ PrimOp PrimOpEq
        | str == "neq#"         = Just $ NamePrim $ PrimOp PrimOpNeq
        | str == "gt#"          = Just $ NamePrim $ PrimOp PrimOpGt
        | str == "lt#"          = Just $ NamePrim $ PrimOp PrimOpLt
        | str == "le#"          = Just $ NamePrim $ PrimOp PrimOpLe
        | str == "and#"         = Just $ NamePrim $ PrimOp PrimOpAnd
        | str == "or#"          = Just $ NamePrim $ PrimOp PrimOpOr

        -- Strings ----------------------------------------
        -- showIntN#
        | Just rest     <- stripPrefix "showInt" str
        , (ds, "#")     <- span isDigit rest
        , bits          <- read ds
        , elem bits [8, 16, 32, 64]
        = Just $ NamePrim $ PrimString $ PrimStringShowInt bits

        -- IO Primops -------------------------------------
        | str == "putStr#"      = Just $ NamePrim $ PrimIO PrimIOPutStr

        -- variables --------------------------------------
        -- Variable names.
        | isLower c      = Just $ NameVar str

        -- literals ---------------------------------------
        | (ds, "")              <- span isDigit str
        = Just $ NameNat (read ds)        


        | otherwise
        = Nothing
