
module DDC.Llvm.Type
        ( -- * Base
          LMString

          -- * Types
        , LlvmType         (..)
        , LlvmAlias

        , isInt
        , isFloat
        , isPointer

          -- * Function Declarations.
        , LlvmFunctionDecl      (..)
        , LlvmFunctionDecls
        , LlvmParameterListType (..)
        , LlvmParameter         (..)
        , LlvmAlign             (..))
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

        LMFunction (LlvmFunctionDecl _ _ _ r varg params _)
         -> let varg' = case varg of
                        VarArgs | null params -> text "..."
                                | otherwise   -> text ", ..."
                        _otherwise            -> empty

                -- by default we don't print param attributes
                args    = hcat $ punctuate comma $ map ppr params

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



-- FunctionDecl -----------------------------------------------------------------------------------
-- | An LLVM Function
data LlvmFunctionDecl 
        = LlvmFunctionDecl 
        { -- | Unique identifier of the function
          decName               :: LMString

          -- | LinkageType of the function
        , decLinkage            :: LlvmLinkageType

        -- | The calling convention of the function
        , decCallConv           :: LlvmCallConvention

        -- | Type of the returned value
        , decReturnType         :: LlvmType

        -- | Indicates if this function uses varargs
        , decParamListType      :: LlvmParameterListType

        -- | Parameter types and attributes
        , decParams             :: [LlvmParameter]

        -- | Function align value, must be power of 2
        , decAlign              :: LlvmAlign }
        deriving (Eq, Show)


type LlvmFunctionDecls 
        = [LlvmFunctionDecl]


-- | Functions can have a fixed amount of parameters, or a variable amount.
data LlvmParameterListType
        = FixedArgs           -- ^ Fixed amount of arguments.
        | VarArgs             -- ^ Variable amount of arguments.
        deriving (Eq,Show)


-- | Describes a function parameter.
data LlvmParameter 
        = LlvmParameter
        { llvmParameterType     :: LlvmType
        , llvmParameterAttrs    :: [LlvmParamAttr] }
        deriving (Show, Eq)

-- | Alignment.
data LlvmAlign    
        = LlvmAlignNone
        | LlvmAlignBytes Int 
        deriving (Show, Eq)

instance Pretty LlvmParameter where
 -- By default we don't print the attrs.
 ppr (LlvmParameter t _attrs)
        = ppr t


instance Pretty LlvmFunctionDecl where
 ppr (LlvmFunctionDecl n l c r varg params a)
  = let varg' = case varg of
                 VarArgs | null params -> text "..."
                         | otherwise   -> text ", ..."
                 _otherwise            -> empty

        align' = case a of
                  LlvmAlignNone     -> empty
                  LlvmAlignBytes a' -> text " align " <+> ppr a'

        args' = hcat $ punctuate comma $ map ppr params

    in  ppr l   <+> ppr c 
                <+> ppr r 
                <+> text " @" 
                <> ppr n <> brackets (args' <> varg') 
                <> align'


