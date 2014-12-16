
module DDC.Llvm.Pretty.Exp
        ( pprPlainX
        , pprPlainL)
where
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Pretty.Type             ()
import DDC.Base.Pretty
import Data.Char
import Numeric
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

        LitString bs    ->  ppr (typeOfLit ll)
                        <+> text "c" <> text (encodeString $ T.unpack bs)

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


-- | Hex encode non-printable characters in this string.
encodeString :: String -> String
encodeString xx
 = "\"" ++ (go [] xx) ++ "\""
 where  go acc []       = reverse acc
        go acc (x : xs)
         |   x == ' '
          || (isAscii x && isAlphaNum x)
          || (isAscii x && isPunctuation x && x /= '"')
         = go (x : acc) xs

         | otherwise
         = go (reverse (encode x) ++ acc) xs

        encode c
         = "\\" ++ (padL $ showHex (ord c) "")

        padL x
         | length x == 0  = "00"
         | length x == 1  = "0"  ++ x
         | otherwise      = x
