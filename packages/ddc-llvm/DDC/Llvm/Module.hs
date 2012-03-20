
module DDC.Llvm.Module
        ( -- * Modules
          Module    (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data.
        , Static    (..)
        , typeOfStatic)
where
import DDC.Llvm.Function
import DDC.Llvm.Var
import DDC.Llvm.Type
import DDC.Base.Pretty


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
        }

-- | A global mutable variable. Maybe defined or external
data Global
        = Global Var (Maybe Static)


-- | Return the 'LlvmType' of the 'LMGlobal'
typeOfGlobal :: Global -> Type
typeOfGlobal (Global v _)       = typeOfVar v


-- | Return the 'LlvmVar' part of a 'LMGlobal'
varOfGlobal :: Global -> Var
varOfGlobal  (Global v _)       = v


-- | Print out a whole LLVM module.
instance Pretty Module where
 ppr (Module _comments aliases _globals _decls funcs)
  =    (vcat $ map ppr aliases)
  <$$> empty
  <$$> (vcat $ map ppr funcs)


-- Static ---------------------------------------------------------------------
-- | Llvm Static Data.
--  These represent the possible global level variables and constants.
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


instance Pretty Static where
  ppr ss
   = case ss of
        StaticComment       s   -> text "; " <> text s
        StaticLit     l         -> ppr l
        StaticUninitType    t   -> ppr t <> text " undef"
        StaticStr     s t       -> ppr t <> text " c\"" <> text s <> text "\\00\""
        StaticArray   d t       -> ppr t <> text " [" <> hcat (punctuate comma $ map ppr d) <> text "]"
        StaticStruct  d t       -> ppr t <> text "<{" <> hcat (punctuate comma $ map ppr d) <> text "}>"
        StaticPointer v         -> ppr v
        StaticBitc    v t       -> ppr t <>  text " bitcast" <+> brackets (ppr v <> text " to " <> ppr t)
        StaticPtoI    v t       -> ppr t <> text " ptrtoint" <+> brackets (ppr v <> text " to " <> ppr t)

        StaticAdd s1 s2
         -> let ty1 = typeOfStatic s1
                op  = if isFloat ty1 then text " fadd (" else text " add ("
            in if ty1 == typeOfStatic s2
                then ppr ty1 <> op <> ppr s1 <> comma <> ppr s2 <> text ")"
                else error $ "LMAdd with different types!"

        StaticSub s1 s2
         -> let ty1 = typeOfStatic s1
                op  = if isFloat ty1 then text " fsub (" else text " sub ("
            in if ty1 == typeOfStatic s2
                then ppr ty1 <> op <> ppr s1 <> comma <> ppr s2 <> text ")"
                else error $ "LMSub with different types!"


