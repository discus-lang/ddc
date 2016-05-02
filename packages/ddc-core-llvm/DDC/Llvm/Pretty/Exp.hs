
module DDC.Llvm.Pretty.Exp
        ( pprPlainX
        , pprPlainL)
where
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Pretty.Type             ()
import DDC.Base.Pretty
import Data.Text                        (Text)
import qualified Data.Text              as T


instance Pretty Exp where
 ppr xx
  = case xx of
        XVar v          -> ppr v
        XLit l          -> ppr l
        XUndef _        -> text "undef"
        XConv _ c x     -> parens $ ppr c <> ppr x

        XGet  _ x is    
         ->  parens $ text "getelementptr" 
         <+> hcat (punctuate (text ", ") (ppr x : map (text . show) is))


-- | Pretty print an expression without its type.
pprPlainX :: Exp -> Doc
pprPlainX xx
 = case xx of
        XVar v          -> ppr $ nameOfVar v
        XLit l          -> pprPlainL l
        XUndef _        -> text "undef"
        XConv _ c x     -> parens $ ppr c <> ppr x

        XGet  _ x is    
         ->  parens $ text "getelementptr"
         <+> hcat (punctuate (text ", ") (ppr x : map (text . show) is))


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
        LitNull  t      -> ppr t <+> text "null"
        LitUndef _      -> text "undef"

        LitString _ txEnc _   
         ->  ppr (typeOfLit ll)
         <+> text "c" <> pprString txEnc



-- | Pretty print a literal without its type.
pprPlainL :: Lit -> Doc
pprPlainL ll
 = case ll of
        LitInt _ i      -> integer i
        LitFloat{}      -> error "ddc-core-llvm.ppr[Lit]: floats aren't handled yet"
        LitNull  _      -> text "null"
        LitUndef _      -> text "undef"

        LitString _ txEnc _  
         -> text "c" <> pprString txEnc


-- | Pretty print an LLVM string.
pprString :: Text -> Doc
pprString tx
 = text "\"" <> text (T.unpack tx) <> text "\""

