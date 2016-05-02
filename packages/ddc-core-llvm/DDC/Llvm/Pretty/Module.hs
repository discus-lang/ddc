{-# LANGUAGE TypeFamilies #-}
module DDC.Llvm.Pretty.Module where
import DDC.Llvm.Syntax.Module
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Pretty.Function 
import DDC.Llvm.Pretty.Exp      ()
import DDC.Llvm.Pretty.Metadata
import DDC.Llvm.Pretty.Base
import DDC.Base.Pretty
import Data.Maybe


-------------------------------------------------------------------------------
-- | Produce a default pretty printer mode from an LLVM config.
prettyModeModuleOfConfig :: Config -> PrettyMode Module
prettyModeModuleOfConfig config
        = PrettyModeModule
        { -- | In LLVM 3.6 the syntax of meta-data was changed from being
          --   similar to that for values to its own untyped syntax. 
          --   If we don't know for sure what the current LLVM version wants
          --   then assume that it wants the new syntax
          modeModuleMetadataAsValue     
                = fromMaybe False 
                $ versionWantsMetadataAsValue 
                $ configLlvmVersion config }


-------------------------------------------------------------------------------
-- | Print out a whole LLVM module.
instance Pretty Module where
 data PrettyMode Module 
        = PrettyModeModule
        { modeModuleMetadataAsValue     :: Bool }

 pprDefaultMode 
        = PrettyModeModule
        { modeModuleMetadataAsValue     = True }   
                -- TODO: Set to False once we test llc version

 pprModePrec 
        (PrettyModeModule asValue) _ 
        (Module _comments aliases globals decls funcs mdecls)
  =     (vcat $ map ppr aliases)
  <$$>  (vcat $ map ppr globals)
  <$$>  (vcat $ map (\decl -> text "declare" 
                          <+> pprFunctionHeader decl Nothing) decls)
  <$$>  empty
  <$$>  (vcat $ punctuate line 
             $ map ppr funcs)
  <$$>  line
  <$$>  empty
  <$$>  (vcat $ map (pprModePrec (PrettyModeMDecl asValue) (0 :: Int)) mdecls)
  <$$>  empty


-------------------------------------------------------------------------------
instance Pretty Global where
 ppr gg
  = case gg of
        GlobalStatic (Var name _) static
         -> ppr name <+> text "= global" <+> ppr static

        GlobalExternal (Var name t)
         -> ppr name <+> text "= external global " <+> ppr t
 

-------------------------------------------------------------------------------
instance Pretty Static where
  ppr ss
   = case ss of
        StaticComment s
         -> text "; " <> text s

        StaticLit l 
         -> ppr l

        StaticUninitType t
         -> ppr t <> text " undef"

        StaticStr   s t
         -> ppr t <> text " c\"" <> text s <> text "\\00\""

        StaticArray d t
         -> ppr t 
         <> text " [" <> hcat (punctuate comma $ map ppr d) <> text "]"

        StaticStruct  d t
         -> ppr t 
         <> text "<{" <> hcat (punctuate comma $ map ppr d) <> text "}>"

        StaticPointer (Var n t)
         -> ppr t     <> text "*" <+> ppr n

        StaticBitc v t
         -> ppr t 
         <> text " bitcast"  <+> parens (ppr v <> text " to " <> ppr t)

        StaticPtoI v t
         -> ppr t 
         <> text " ptrtoint" <+> parens (ppr v <> text " to " <> ppr t)

        StaticAdd s1 s2
         -> let ty1 = typeOfStatic s1
                op  = if isFloat ty1 then text " fadd (" else text " add ("
            in if ty1 == typeOfStatic s2
                then ppr ty1 <> op <> ppr s1 <> comma <> ppr s2 <> text ")"
                else error $ "ddc-core-llvm: LMAdd with different types!"

        StaticSub s1 s2
         -> let ty1 = typeOfStatic s1
                op  = if isFloat ty1 then text " fsub (" else text " sub ("
            in if ty1 == typeOfStatic s2
                then ppr ty1 <> op <> ppr s1 <> comma <> ppr s2 <> text ")"
                else error $ "ddc-core-llvm: LMSub with different types!"

