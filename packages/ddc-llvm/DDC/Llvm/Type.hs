
module DDC.Llvm.Type
        ( module DDC.Llvm.Attr

          -- * Function Declarations.
        , FunctionDecl  (..)
        , ParamListType (..)
        , Param         (..)
        , Align         (..)

          -- * Types
        , Type          (..)
        , TypeAlias     (..)

        , isInt
        , isFloat
        , isPointer
        , takeBytesOfType)
where
import DDC.Llvm.Attr
import DDC.Base.Pretty
import Control.Monad


-- FunctionDecl -----------------------------------------------------------------------------------
-- | An LLVM Function
data FunctionDecl 
        = FunctionDecl 
        { -- | Unique identifier of the function
          declName              :: String

          -- | LinkageType of the function
        , declLinkage           :: Linkage

        -- | The calling convention of the function
        , declCallConv          :: CallConv

        -- | Type of the returned value
        , declReturnType        :: Type

        -- | Indicates if this function uses varargs
        , declParamListType     :: ParamListType

        -- | Parameter types and attributes
        , declParams            :: [Param]

        -- | Function align value, must be power of 2
        , declAlign             :: Align }
        deriving (Eq, Show)


-- | Functions can have a fixed amount of parameters, or a variable amount.
data ParamListType
        = FixedArgs           -- ^ Fixed amount of arguments.
        | VarArgs             -- ^ Variable amount of arguments.
        deriving (Eq,Show)


-- | Describes a function parameter.
data Param 
        = Param
        { paramType         :: Type
        , paramAttrs        :: [ParamAttr] }
        deriving (Show, Eq)


-- | Alignment.
data Align
        = AlignNone
        | AlignBytes Integer
        deriving (Show, Eq)


instance Pretty Param where
 -- By default we don't print the attrs.
 ppr (Param t _attrs)
        = ppr t


instance Pretty FunctionDecl where
 ppr (FunctionDecl n l c r varg params a)
  = let varg' = case varg of
                 VarArgs | null params  -> text "..."
                         | otherwise    -> text ", ..."
                 _otherwise             -> empty

        align' = case a of
                  AlignNone         -> empty
                  AlignBytes a'     -> text " align " <+> ppr a'

        args' = hcat $ punctuate comma $ map ppr params

    in  ppr l   <+> ppr c 
                <+> ppr r 
                <+> text " @" 
                <> ppr n <> brackets (args' <> varg') 
                <> align'


-- TypeAlias --------------------------------------------------------------------------------------
-- | A type alias.
data TypeAlias 
        = TypeAlias     String Type
        deriving (Eq, Show)


instance Pretty TypeAlias where
 ppr (TypeAlias name ty)
        = text "%" <> text name <+> equals <+> text "type" <+> ppr ty


-- Type -------------------------------------------------------------------------------------------
-- | Llvm Types.
data Type
        = TInt          Integer         -- ^ An integer with a given width in bits.
        | TFloat                        -- ^ 32 bit floating point
        | TDouble                       -- ^ 64 bit floating point
        | TFloat80                      -- ^ 80 bit (x86 only) floating point
        | TFloat128                     -- ^ 128 bit floating point
        | TPointer      Type            -- ^ A pointer to a 'LlvmType'
        | TArray        Integer Type    -- ^ An array of 'LlvmType'
        | TLabel                        -- ^ A 'LlvmVar' can represent a label (address)
        | TVoid                         -- ^ Void type
        | TStruct       [Type]          -- ^ Structure type
        | TAlias        TypeAlias       -- ^ A type alias
        | TFunction     FunctionDecl    -- ^ Function type, used to create pointers to functions
        deriving (Eq, Show)


instance Pretty Type where
 ppr lt
  = case lt of
        TInt size      -> text "i" <> integer size
        TFloat         -> text "float"
        TDouble        -> text "double"
        TFloat80       -> text "x86_fp80"
        TFloat128      -> text "fp128"
        TPointer x     -> ppr x <> text "*"
        TArray nr tp   -> brackets (integer nr <> text " x " <> ppr tp)
        TLabel         -> text "label"
        TVoid          -> text "void"
        TStruct tys    -> text "<{" <> (hcat $ punctuate comma (map ppr tys)) <> text "}>"

        TAlias (TypeAlias s _)  
         -> text "%" <> text s

        TFunction (FunctionDecl _ _ _ r varg params _)
         -> let varg' = case varg of
                        VarArgs | null params -> text "..."
                                | otherwise   -> text ", ..."
                        _otherwise            -> empty

                -- by default we don't print param attributes
                args    = hcat $ punctuate comma $ map ppr params

            in ppr r <> brackets (args <> varg')


-- | Test if the given 'LlvmType' is an integer
isInt :: Type -> Bool
isInt tt
 = case tt of
        TInt _          -> True
        _               -> False


-- | Test if the given 'LlvmType' is a floating point type
isFloat :: Type -> Bool
isFloat tt
 = case tt of
        TFloat          -> True
        TDouble         -> True
        TFloat80        -> True
        TFloat128       -> True
        _               -> False


-- | Test if the given 'LlvmType' is an 'LMPointer' construct
isPointer :: Type -> Bool
isPointer tt
 = case tt of
        TPointer _      -> True
        _               -> False


-- | Calculate the size in bytes of a Type, given the size of pointers.
takeBytesOfType :: Integer -> Type -> Maybe Integer
takeBytesOfType bytesPtr tt
 = case tt of
        TInt bits       -> Just $ fromIntegral $ div bits 8
        TFloat          -> Just 4
        TDouble         -> Just 8
        TFloat80        -> Just 10
        TFloat128       -> Just 16
        TPointer{}      -> Just bytesPtr

        TArray n t
         | Just s <- takeBytesOfType bytesPtr t
         -> Just (n * s)

        TLabel{}        -> Nothing
        TVoid{}         -> Nothing

        TStruct tys     
         -> liftM sum $ sequence $ map (takeBytesOfType bytesPtr) tys

        TAlias (TypeAlias _ t)
         -> takeBytesOfType bytesPtr t

        _               -> Nothing
