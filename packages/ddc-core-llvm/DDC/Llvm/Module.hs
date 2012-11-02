
module DDC.Llvm.Module
        ( -- * Modules
          Module    (..)
        , Global    (..)
        , typeOfGlobal
        , varOfGlobal

          -- * Static data.
        , Static    (..)
        , typeOfStatic)
where
import DDC.Llvm.Function
import DDC.Llvm.Exp
import DDC.Llvm.Metadata
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
        
          -- | Metdata for alias analysis
        , modMDecls    :: [MDecl]
        }


-- | Print out a whole LLVM module.
instance Pretty Module where
 ppr (Module _comments aliases globals decls funcs mdecls)
  =    (vcat $ map ppr aliases)
  <$$> (vcat    $ map ppr globals)
  <$$> (vcat    $ map (\decl ->  text "declare" 
                             <+> pprFunctionHeader decl Nothing) decls)
  <$$> empty
  <$$> (vcat    $ punctuate line 
                $ map ppr funcs)
  <$$> line
  <$$> empty
  <$$> (vcat    $ map ppr mdecls)


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


instance Pretty Global where
 ppr gg
  = case gg of
        GlobalStatic (Var name _) static
         -> ppr name <+> text "= global" <+> ppr static

        GlobalExternal (Var name t)
         -> ppr name <+> text "= external global " <+> ppr t
 

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


instance Pretty Static where
  ppr ss
   = case ss of
        StaticComment       s    -> text "; " <> text s
        StaticLit     l          -> ppr l
        StaticUninitType    t    -> ppr t <> text " undef"
        StaticStr     s t        -> ppr t <> text " c\"" <> text s <> text "\\00\""
        StaticArray   d t        -> ppr t <> text " [" <> hcat (punctuate comma $ map ppr d) <> text "]"
        StaticStruct  d t        -> ppr t <> text "<{" <> hcat (punctuate comma $ map ppr d) <> text "}>"
        StaticPointer (Var n t)  -> ppr t <> text "*" <+> ppr n
        StaticBitc    v t        -> ppr t <> text " bitcast"  <+> parens (ppr v <> text " to " <> ppr t)
        StaticPtoI    v t        -> ppr t <> text " ptrtoint" <+> parens (ppr v <> text " to " <> ppr t)

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


