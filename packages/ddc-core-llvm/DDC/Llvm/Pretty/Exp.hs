
module DDC.Llvm.Pretty.Exp
        ( pprPlainX
        , pprPlainL)
where
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Pretty.Type     ()
import DDC.Base.Pretty


instance Pretty Exp where
 ppr xx
  = case xx of
        XVar v   -> ppr v
        XLit l   -> ppr l
        XUndef _ -> text "undef"


-- | Pretty print an expression without its type.
pprPlainX :: Exp -> Doc
pprPlainX xx
 = case xx of
        XVar v   -> ppr $ nameOfVar v
        XLit l   -> pprPlainL l
        XUndef _ -> text "undef"


instance Pretty Var where
 ppr (Var n t)  = ppr t <+> ppr n


instance Pretty Name where
 ppr (NameGlobal str)   = text "@" <> text str
 ppr (NameLocal  str)   = text "%" <> text str


instance Pretty Lit where
 ppr ll
  = case ll of
        LitInt   t i    -> ppr t <+> integer i
        LitFloat{}      -> error "ddc-core-llvm.ppr[Lit]: floats aren't handled yet"
        LitString bs    -> text "c" <> text (show bs)
                                        -- TODO: pretty print special chars for strings,
                                        -- LLVM uses hex escapes eg \00
        LitNull  t      -> ppr t <+> text "null"
        LitUndef _      -> text "undef"


-- | Pretty print a literal without its type.
pprPlainL :: Lit -> Doc
pprPlainL ll
 = case ll of
        LitInt _ i      -> integer i
        LitFloat{}      -> error "ddc-core-llvm.ppr[Lit]: floats aren't handled yet"
        LitString bs    -> text "c" <> text (show bs)
        LitNull  _      -> text "null"
        LitUndef _      -> text "undef"

