
module DDC.Llvm.Lit
        ( Lit(..)
        , pprPlainL
        , typeOfLit )
where
import DDC.Llvm.Type
import DDC.Base.Pretty


-- | Literal data.
data Lit
        -- | An integer literal
        = LitInt        Type    Integer

        -- | A floating-point literal.
        | LitFloat      Type    Double

        -- | A null pointer literal.
        --   Only applicable to pointer types
        | LitNull       Type

        -- | A completely undefined value.
        | LitUndef      Type
        deriving (Eq, Show)


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


-- | Yield the `Type` of a `Lit`.
typeOfLit :: Lit -> Type
typeOfLit ll
 = case ll of
        LitInt    t _   -> t
        LitFloat  t _   -> t
        LitNull   t     -> t
        LitUndef  t     -> t
