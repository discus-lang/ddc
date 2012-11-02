
module DDC.Llvm.Exp
        ( module DDC.Llvm.Type

          -- * Expressions
        , Exp   (..)
        , pprPlainX
        , typeOfExp

          -- * Variables
        , Var   (..)
        , nameOfVar
        , typeOfVar

          -- * Names
        , Name  (..)

          -- * Literals
        , Lit   (..)
        , pprPlainL
        , typeOfLit)
where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Base.Pretty


-- Exp ------------------------------------------------------------------------
data Exp 
        -- | Use of a variable.
        = XVar   Var

        -- | A literal.
        | XLit   Lit

        -- | An undefined value.
        | XUndef Type
        deriving (Eq, Show)  


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


-- | Take the type of an expression.
typeOfExp :: Exp -> Type 
typeOfExp xx
 = case xx of
        XVar   var      -> typeOfVar var
        XLit   lit      -> typeOfLit lit
        XUndef t        -> t


-- Var ------------------------------------------------------------------------
-- | A variable that can be assigned to.
data Var
        = Var   Name    Type
        deriving (Eq, Show)


-- | Yield the name of a var.
nameOfVar :: Var -> Name
nameOfVar (Var n _)     = n


-- | Yield the type of a var.
typeOfVar :: Var -> Type
typeOfVar (Var _ t)     = t


instance Pretty Var where
 ppr (Var n t)  = ppr t <+> ppr n

instance Ord Var where
 compare (Var n1 _) (Var n2 _)
        = compare n1 n2


-- Name -----------------------------------------------------------------------
-- | Names of variables.
data Name
        = NameGlobal String
        | NameLocal  String
        deriving (Show, Eq, Ord)

instance Pretty Name where
 ppr (NameGlobal str)   = text "@" <> text str
 ppr (NameLocal  str)   = text "%" <> text str



-- Lit ------------------------------------------------------------------------
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


