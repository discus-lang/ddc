
module DDC.Llvm.Pretty.Exp
        ( pprPlainX
        , pprPlainL)
where
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Pretty.Type     ()
import DDC.Base.Pretty


-- Exp ------------------------------------------------------------------------
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


-- Var ------------------------------------------------------------------------
instance Pretty Var where
 ppr (Var n t)  = ppr t <+> ppr n


-- Name -----------------------------------------------------------------------
instance Pretty Name where
 ppr (NameGlobal str)   = text "@" <> text str
 ppr (NameLocal  str)   = text "%" <> text str


-- Lit ------------------------------------------------------------------------
instance Pretty Lit where
 ppr ll
  = case ll of
        LitInt   t i    -> ppr t <+> integer i
        LitFloat{}      -> error "ppr[Lit]: floats aren't handled yet"
        LitNull  _      -> text "null"
        LitUndef _      -> text "undef"


-- | Pretty print a literal without its type.
pprPlainL :: Lit -> Doc
pprPlainL ll
 = case ll of
        LitInt _ i      -> integer i
        LitFloat{}      -> error "ppr[Lit]: floats aren't handled yet"
        LitNull  _      -> text "null"
        LitUndef _      -> text "undef"

