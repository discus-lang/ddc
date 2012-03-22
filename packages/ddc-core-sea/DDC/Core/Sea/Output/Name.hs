
-- | Names used in the SeaOutput language profile.
--   These map directly onto names used in the C output language.
module DDC.Core.Sea.Output.Name
        ( Name            (..)
        , Prim            (..)
        , PrimTyCon       (..)
        , PrimOp          (..)
        , PrimCast        (..)
        , PrimCall        (..)
        , PrimControl     (..)
        , PrimStore       (..)
        , PrimStoreLayout (..)
        , PrimExternal    (..)
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

        -- | A natural number literal.
        | NameNat       Integer

        -- | A constructor tag literal.
        | NameTag       Integer

        -- | A boolean literal.
        | NameBool      Bool
        deriving (Eq, Ord, Show)


instance Pretty Name where
 ppr nn
  = case nn of
        NameObjTyCon      -> text "Obj"
        NamePrimTyCon tc  -> ppr tc
        NameVar  n        -> text n
        NamePrim p        -> ppr p
        NameNat  i        -> integer i
        NameTag  i        -> text "TAG" <> integer i
        NameBool True     -> text "True#"
        NameBool False    -> text "False#"


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
        = PrimCastOp
        | PrimCastNatToInt Int
        deriving (Eq, Ord, Show)


instance Pretty PrimCast where
 ppr c
  = case c of
        PrimCastOp      -> text "cast#"

        PrimCastNatToInt bits
         -> text "i" <> int bits <> text "#"


-- PrimCall -------------------------------------------------------------------
-- | Primitive ways of invoking a function, 
--   where control flow returns back to the caller.
data PrimCall
        -- | Tailcall a function
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


-- Store -----------------------------------------------------------------------
-- | A projection of some other object.
data PrimStore
        -- | Read a value from the store.
        = PrimStoreRead

        -- | Write a value to the store.
        | PrimStoreWrite

        -- | Take the tag of a boxed object.
        | PrimStoreProjTag

        -- | Take a numbered field from some boxed data object.
        | PrimStoreProjField    PrimStoreLayout

        -- | Allocate a fresh Data object.
        | PrimStoreAllocData    PrimStoreLayout
        deriving (Eq, Ord, Show)


-- | Possible layout of objects.
data PrimStoreLayout
        = PrimStoreLayoutRaw
        | PrimStoreLayoutBoxed
        | PrimStoreLayoutMixed
        deriving (Eq, Ord, Show)


instance Pretty PrimStore where
 ppr p
  = case p of        
        PrimStoreRead             -> text "read#"
        PrimStoreWrite            -> text "write#"

        PrimStoreProjTag          -> text "tag#"
        PrimStoreProjField layout -> text "field" <> ppr layout <> text "#"

        PrimStoreAllocData layout -> text "alloc" <> ppr layout <> text "#"


instance Pretty PrimStoreLayout where
 ppr layout
  = case layout of
        PrimStoreLayoutRaw      -> text "Raw"
        PrimStoreLayoutBoxed    -> text "Boxed"
        PrimStoreLayoutMixed    -> text "Mixed"


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

        -- Arithmetic and Bitwise -------------------------
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


        -- Casts ------------------------------------------
        -- Cast Nat to Int
        | Just rest     <- stripPrefix "i" str
        , (ds, "#")     <- span isDigit rest
        , bits          <- read ds
        , elem bits [8, 16, 32, 64]
        = Just $ NamePrim $ PrimCast $ PrimCastNatToInt bits

        | str == "cast#"        = Just $ NamePrim $ PrimCast    PrimCastOp


        -- Calls ------------------------------------------
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

        | str == "force#"       = Just $ NamePrim $ PrimCall    PrimCallForce


        -- Control ----------------------------------------
        | str == "fail#"        = Just $ NamePrim $ PrimControl PrimControlFail
        | str == "return#"      = Just $ NamePrim $ PrimControl PrimControlReturn


        -- Store ------------------------------------------
        | str == "read#"        = Just $ NamePrim $ PrimStore PrimStoreRead
        | str == "write#"       = Just $ NamePrim $ PrimStore PrimStoreWrite

        | str == "tag#"         = Just $ NamePrim $ PrimStore PrimStoreProjTag

        | str == "fieldRaw#"    
        = Just $ NamePrim $ PrimStore (PrimStoreProjField PrimStoreLayoutRaw)

        | str == "fieldBoxed#"  
        = Just $ NamePrim $ PrimStore (PrimStoreProjField PrimStoreLayoutBoxed)

        | str == "fieldRaw#"    
        = Just $ NamePrim $ PrimStore (PrimStoreProjField PrimStoreLayoutMixed)

        | str == "allocRaw#"
        = Just $ NamePrim $ PrimStore (PrimStoreAllocData PrimStoreLayoutRaw)

        | str == "allocBoxed#"  
        = Just $ NamePrim $ PrimStore (PrimStoreAllocData PrimStoreLayoutBoxed)

        | str == "allocMixed#"
        = Just $ NamePrim $ PrimStore (PrimStoreAllocData PrimStoreLayoutMixed)

        -- External ----------------------------------------
        -- showIntN#
        | Just rest     <- stripPrefix "showInt" str
        , (ds, "#")     <- span isDigit rest
        , bits          <- read ds
        , elem bits [8, 16, 32, 64]
        = Just $ NamePrim $ PrimExternal $ PrimExternalShowInt bits

        | str == "putStr#"      = Just $ NamePrim $ PrimExternal PrimExternalPutStr
        | str == "putStrLn#"    = Just $ NamePrim $ PrimExternal PrimExternalPutStrLn

        -- variables --------------------------------------
        -- Variable names.
        | isLower c      = Just $ NameVar str

        -- literals ---------------------------------------
        -- Naturals
        | (ds, "")              <- span isDigit str
        = Just $ NameNat (read ds)        

        -- Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ NameTag (read ds)

        -- Bools
        | str == "True#"        = Just $ NameBool True
        | str == "False#"       = Just $ NameBool False

        | otherwise
        = Nothing
