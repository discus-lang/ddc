
module DDC.Llvm.Pretty.Function
        ( pprFunctionHeader)
where
import DDC.Llvm.Syntax.Function
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Pretty.Attr             ()
import DDC.Llvm.Pretty.Instr            ()
import DDC.Base.Pretty


instance Pretty Function where
 ppr (Function decl paramNames attrs sec body) 
  = let attrDoc = hsep $ map ppr attrs
        secDoc  = case sec of
                        SectionAuto       -> empty
                        SectionSpecific s -> text "section" <+> (dquotes $ text s)

    in text "define" 
        <+> pprFunctionHeader decl (Just paramNames)
                <+> attrDoc <+> secDoc
        <$> lbrace
        <$> vcat (map ppr body)
        <$> rbrace


-- | Print out a function defenition header.
pprFunctionHeader :: FunctionDecl -> Maybe [String] -> Doc
pprFunctionHeader 
        (FunctionDecl name linkage callConv tReturn varg params alignment)
        mnsParams
  = let varg'  = case varg of
                      VarArgs | null params -> text "..."
                              | otherwise   -> text ", ..."
                      _otherwise            -> empty

        align' = case alignment of
                        AlignNone       -> empty
                        AlignBytes b    -> text " align" <+> ppr b

        args'  
         = case mnsParams of
             Just nsParams      
              -> [ ppr ty <+> hsep (map ppr attrs) <+> text "%" <> text nParam
                        | Param ty attrs <- params
                        | nParam         <- nsParams ]

             Nothing
              -> [ ppr ty <+> hsep (map ppr attrs)
                        | Param ty attrs <- params ]

    in ppr linkage
        <+> ppr callConv
        <+> ppr tReturn
        <+> text "@" <> text name
        <>  lparen 
        <>  (hcat $ punctuate (comma <> space) args') <> varg' 
        <>  rparen 
        <>  align'
