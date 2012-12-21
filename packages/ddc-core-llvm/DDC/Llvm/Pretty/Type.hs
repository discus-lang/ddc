
module DDC.Llvm.Pretty.Type where
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Pretty.Attr     ()
import DDC.Base.Pretty


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


instance Pretty TypeAlias where
 ppr (TypeAlias name ty)
        = text "%" <> text name <+> equals <+> text "type" <+> ppr ty


instance Pretty Type where
 ppr lt
  = case lt of
        TVoid          -> text "void"
        TInt size      -> text "i" <> integer size
        TFloat         -> text "float"
        TDouble        -> text "double"
        TFloat80       -> text "x86_fp80"
        TFloat128      -> text "fp128"
        TLabel         -> text "label"
        TPointer x     -> ppr x <> text "*"

        TStruct tys
         -> text "<{" <> (hcat $ punctuate comma (map ppr tys)) <> text "}>"

        TArray nr tp
         -> brackets (integer nr <> text " x " <> ppr tp)

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




