
module DDC.Llvm.Syntax.Module
        ( -- * Modules
          Module    (..)
        , lookupCallConv

        , Global    (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data.
        , Static    (..)
        , typeOfStatic)
where
import DDC.Llvm.Syntax.Function
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Attr
import Data.List
import Control.Monad


-- Module ---------------------------------------------------------------------
-- | This is a top level container in LLVM.
data Module
        = Module  
        { -- | Comments to include at the start of the module.
          modComments  :: [String]

          -- | Alias type definitions.
        , modAliases   :: [TypeAlias]

          -- | Global variables to include in the module.
        , modGlobals   :: [Global]

          -- | Functions used in this module but defined in other modules.
        , modFwdDecls  :: [FunctionDecl]

          -- | Functions defined in this module.
        , modFuncs     :: [Function]
        
          -- | Metdata for alias analysis
        , modMDecls    :: [MDecl]
        }


-- | Lookup the calling convention for this function,
--   using the forward declarations as well as the function definitions.
lookupCallConv :: String -> Module -> Maybe CallConv
lookupCallConv name mm
        = liftM declCallConv
        $ find isFunctionDecl $ modFwdDecls mm ++ (map funDecl $ modFuncs mm)
        where isFunctionDecl decl
                = declName decl == name


-- Global ---------------------------------------------------------------------
-- | A global mutable variable. Maybe defined or external
data Global
        = GlobalStatic   Var Static
        | GlobalExternal Var 


-- | Return the 'LlvmType' of the 'LMGlobal'
typeOfGlobal :: Global -> Type
typeOfGlobal gg
 = case gg of
        GlobalStatic v _        -> typeOfVar v
        GlobalExternal v        -> typeOfVar v


-- | Return the 'LlvmVar' part of a 'LMGlobal'
varOfGlobal :: Global -> Var
varOfGlobal gg
 = case gg of
        GlobalStatic v _        -> v
        GlobalExternal v        -> v


-- Static ---------------------------------------------------------------------
-- | Llvm Static Data.
--   These represent the possible global level variables and constants.
data Static
        -- | A comment in a static section.
        = StaticComment       String

        -- | A static variant of a literal value.
        | StaticLit             Lit

        -- | For uninitialised data.
        | StaticUninitType      Type

        -- | Defines a static 'LMString'.
        | StaticStr             String   Type

        -- | A static array.
        | StaticArray           [Static] Type

        -- | A static structure type.
        | StaticStruct          [Static] Type

        -- | A pointer to other data.
        | StaticPointer         Var

        -- Static expressions.
        -- | Pointer to Pointer conversion.
        | StaticBitc            Static Type                    

        -- | Pointer to Integer conversion.
        | StaticPtoI            Static Type                    

        -- | Constant addition operation.
        | StaticAdd             Static Static                 

        -- | Constant subtraction operation.
        | StaticSub             Static Static  
        deriving (Show)                


-- | Return the 'LlvmType' of the 'LlvmStatic'.
typeOfStatic :: Static -> Type
typeOfStatic ss
 = case ss of
        StaticComment{}         -> error "Can't call getStatType on LMComment!"
        StaticLit   l           -> typeOfLit l
        StaticUninitType t      -> t
        StaticStr    _ t        -> t
        StaticArray  _ t        -> t
        StaticStruct _ t        -> t
        StaticPointer v         -> typeOfVar v
        StaticBitc   _ t        -> t
        StaticPtoI   _ t        -> t
        StaticAdd    t _        -> typeOfStatic t
        StaticSub    t _        -> typeOfStatic t


