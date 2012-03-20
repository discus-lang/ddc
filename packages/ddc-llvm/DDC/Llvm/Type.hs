
module DDC.Llvm.Type
        ( -- * Base
          LMString

          -- * Types
        , LlvmType         (..)
        , LlvmAlias
        , isInt
        , isFloat
        , isPointer
        , llvmWidthInBits
        , pLift
        , pLower
        , i128, i64, i32, i16, i8, i1
        , i8Ptr

          -- * Function Declarations.
        , LlvmFunctionDecl (..)
        , LlvmFunctionDecls
        , LlvmParameterListType
        , LlvmParameter
        , LMAlign)
where
import DDC.Llvm.Attr
import DDC.Base.Pretty

-- Base -------------------------------------------------------------------------------------------
-- | A String in LLVM
type LMString 
        = String


-- Type -------------------------------------------------------------------------------------------
-- | Llvm Types.
data LlvmType
        = LMInt Int                     -- ^ An integer with a given width in bits.
        | LMFloat                       -- ^ 32 bit floating point
        | LMDouble                      -- ^ 64 bit floating point
        | LMFloat80                     -- ^ 80 bit (x86 only) floating point
        | LMFloat128                    -- ^ 128 bit floating point
        | LMPointer       LlvmType      -- ^ A pointer to a 'LlvmType'
        | LMArray     Int LlvmType      -- ^ An array of 'LlvmType'
        | LMLabel                       -- ^ A 'LlvmVar' can represent a label (address)
        | LMVoid                        -- ^ Void type
        | LMStruct   [LlvmType]         -- ^ Structure type
        | LMAlias    LlvmAlias          -- ^ A type alias
        | LMFunction LlvmFunctionDecl   -- ^ Function type, used to create pointers to functions
        deriving (Eq, Show)


-- | A type alias
type LlvmAlias 
        = (LMString, LlvmType)


instance Pretty LlvmType where
 ppr lt
  = case lt of
        LMInt size      -> text "i" <> int size
        LMFloat         -> text "float"
        LMDouble        -> text "double"
        LMFloat80       -> text "x86_fp80"
        LMFloat128      -> text "fp128"
        LMPointer x     -> ppr x <> text "*"
        LMArray nr tp   -> brackets (int nr <> text " x " <> ppr tp)
        LMLabel         -> text "label"
        LMVoid          -> text "void"
        LMStruct tys    -> text "<{" <> (hcat $ punctuate comma (map ppr tys)) <> text "}>"

        LMFunction (LlvmFunctionDecl _ _ _ r varg p _)
         -> let varg' = case varg of
                        VarArgs | null p    -> text "..."
                                | otherwise -> text ", ..."
                        _otherwise          -> empty

                -- by default we don't print param attributes
                args    = hcat $ punctuate comma $ map (ppr . fst) p

            in ppr r <> brackets (args <> varg')

        LMAlias (s, _)  -> text "%" <> ppr s


-- | Test if the given 'LlvmType' is an integer
isInt :: LlvmType -> Bool
isInt tt
 = case tt of
        LMInt _         -> True
        _               -> False


-- | Test if the given 'LlvmType' is a floating point type
isFloat :: LlvmType -> Bool
isFloat tt
 = case tt of
        LMFloat         -> True
        LMDouble        -> True
        LMFloat80       -> True
        LMFloat128      -> True
        _               -> False


-- | Test if the given 'LlvmType' is an 'LMPointer' construct
isPointer :: LlvmType -> Bool
isPointer tt
 = case tt of
        LMPointer _     -> True
        _               -> False


-- | Width in bits of an 'LlvmType', returns 0 if not applicable        -- TODO: make this return Nothing
llvmWidthInBits :: LlvmType -> Int
llvmWidthInBits tt
 = case tt of
        LMInt n         -> n
        LMFloat         -> 32
        LMDouble        -> 64
        LMFloat80       -> 80
        LMFloat128      -> 128

        -- Could return either a pointer width here or the width of what
        -- it points to. We will go with the former for now.
        LMPointer{}     -> llvmWidthInBits llvmWord
        LMArray{}       -> llvmWidthInBits llvmWord
        LMLabel         -> 0
        LMVoid          -> 0
        LMStruct tys    -> sum $ map llvmWidthInBits tys
        LMFunction{}    -> 0
        LMAlias (_,t)   -> llvmWidthInBits t


-- | The target architectures word size                                 -- TODO: cleanup this mess
llvmWord :: LlvmType
llvmWord    = LMInt (wORD_SIZE * 8)

wORD_SIZE       = 32                                                    -- TODO: parameterise this


-- | Add a pointer indirection to the supplied type. 'LMLabel' and 'LMVoid'
-- cannot be lifted.
pLift :: LlvmType -> LlvmType
pLift (LMLabel) = error "Labels are unliftable"
pLift (LMVoid)  = error "Voids are unliftable"
pLift x         = LMPointer x


-- | Remove the pointer indirection of the supplied type. Only 'LMPointer'
-- constructors can be lowered.
pLower :: LlvmType -> LlvmType
pLower (LMPointer x) = x
pLower x  = error $ show x ++ " is a unlowerable type, need a pointer"


-- | Shortcut for Common Types
i128, i64, i32, i16, i8, i1, i8Ptr :: LlvmType
i128  = LMInt 128
i64   = LMInt  64
i32   = LMInt  32
i16   = LMInt  16
i8    = LMInt   8
i1    = LMInt   1
i8Ptr = pLift i8




-- FunctionDecl -----------------------------------------------------------------------------------
-- | An LLVM Function
data LlvmFunctionDecl 
        = LlvmFunctionDecl 
        { -- | Unique identifier of the function
          decName       :: LMString

          -- | LinkageType of the function
        , funcLinkage   :: LlvmLinkageType

        -- | The calling convention of the function
        , funcCc        :: LlvmCallConvention

        -- | Type of the returned value
        , decReturnType :: LlvmType

        -- | Indicates if this function uses varargs
        , decVarargs    :: LlvmParameterListType

        -- | Parameter types and attributes
        , decParams     :: [LlvmParameter]

        -- | Function align value, must be power of 2
        , funcAlign     :: LMAlign }
        deriving (Eq, Show)


type LlvmFunctionDecls 
        = [LlvmFunctionDecl]


-- | Functions can have a fixed amount of parameters, or a variable amount.
data LlvmParameterListType
        = FixedArgs           -- ^ Fixed amount of arguments.
        | VarArgs             -- ^ Variable amount of arguments.
        deriving (Eq,Show)


-- | Describes a function parameter.
type LlvmParameter 
        = (LlvmType, [LlvmParamAttr])

-- | Alignment.
type LMAlign    = Maybe Int


instance Pretty LlvmFunctionDecl where
 ppr (LlvmFunctionDecl n l c r varg p a)
  = let varg' = case varg of
                 VarArgs | null args -> text "..."
                         | otherwise -> text ", ..."
                 _otherwise          -> empty

        align' = case a of
                 Just a' -> text " align " <+> ppr a'
                 Nothing -> empty

        -- by default we don't print param attributes
        args  = map (show . fst) p
        args' = hcat $ punctuate comma $ map ppr args

    in  ppr l <+> ppr c <+> ppr r <+> text " @" <> ppr n <>
                brackets (args' <> varg') 
                <> align'


