{-# LANGUAGE TypeFamilies #-}
module DDC.Llvm.Pretty.Function where
import DDC.Llvm.Syntax.Function
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Pretty.Attr             ()
import DDC.Llvm.Pretty.Instr
import DDC.Llvm.Pretty.Base
import DDC.Data.Pretty
import Prelude                          hiding ((<$>))


instance Pretty Function where
 data PrettyMode Function
        = PrettyModeFunction
        { modeFunctionConfig :: Config }

 pprDefaultMode
        = PrettyModeFunction
        { modeFunctionConfig = defaultConfig }

 pprModePrec (PrettyModeFunction config) prec
        (Function decl paramNames attrs sec body)
  = let
        attrDoc  = hsep $ map ppr attrs

        secDoc   = case sec of
                        SectionAuto       -> mempty
                        SectionSpecific s -> text "section" <+> (dquotes $ text s)

        pprBlock = pprModePrec (PrettyModeBlock config) prec

    in vcat
        [ text "define"
                <+> pprFunctionHeader decl (Just paramNames)
                        <+> attrDoc <+> secDoc
        , lbrace
        , vcat (map pprBlock body)
        , rbrace ]


-- | Print out a function defenition header.
pprFunctionHeader :: FunctionDecl -> Maybe [String] -> Doc
pprFunctionHeader
        (FunctionDecl name linkage callConv tReturn varg params alignment strategy)
        mnsParams
  = let varg'  = case varg of
                      VarArgs | null params -> text "..."
                              | otherwise   -> text ", ..."
                      _otherwise            -> mempty

        align' = case alignment of
                        AlignNone       -> mempty
                        AlignBytes b    -> text " align" <+> ppr b

        gc'    = case strategy of
                        Nothing         -> mempty
                        Just strategy'  -> text " gc" <+> dquotes (text strategy')

        args'
         = case mnsParams of
             Just nsParams
              -> [ ppr ty <+> hsep (map ppr attrs) <+> text "%" <> text nParam
                        | Param ty attrs <- params
                        | nParam         <- nsParams ]

             Nothing
              -> [ ppr ty <+> hsep (map ppr attrs)
                        | Param ty attrs <- params ]

        convSuffix' 0 = mempty
        convSuffix' _ = space

        convSpace'
         = width (ppr callConv) convSuffix'

    in ppr linkage
        <+> convSpace'
        <>  ppr tReturn
        <+> text "@" <> text name
        <>  lparen
        <>  (hcat $ punctuate (comma <> space) args') <> varg'
        <>  rparen
        <>  align'
        <>  gc'
