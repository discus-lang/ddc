
module DDC.Llvm.Syntax.Type
        ( -- * Function Declarations.
          FunctionDecl  (..)
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
import DDC.Llvm.Syntax.Attr
import Control.Monad


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


-- | A type alias.
data TypeAlias 
        = TypeAlias     String Type
        deriving (Eq, Show)


-- | Llvm Types.
data Type
        -- | Void type
        = TVoid                         

        -- | An integer with a given width in bits.
        | TInt          Integer         

        -- | 32-bit floating point
        | TFloat

        -- | 64-bit floating point
        | TDouble                       

        -- | 80 bit (x86 only) floating point
        | TFloat80                      

        -- | 128 bit floating point
        | TFloat128                     

        -- |  A block label.
        | TLabel                       

        -- | A pointer to another type of thing.
        | TPointer      Type           

        -- | An array of things.
        | TArray        Integer Type

        -- | A structure type.
        | TStruct       [Type]

        -- | A type alias.
        | TAlias        TypeAlias

        -- | Function type, used to create pointers to functions.
        | TFunction     FunctionDecl
        deriving (Eq, Show)


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
