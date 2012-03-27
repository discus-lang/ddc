
module DDC.Llvm.Exp
        ( module DDC.Llvm.Type
        , module DDC.Llvm.Lit

          -- * Names
        , Name  (..)

          -- * Variables
        , Var   (..)
        , nameOfVar
        , typeOfVar

          -- * Expressions
        , Exp   (..)
        , pprPlainX
        , typeOfExp)
where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Llvm.Lit
import DDC.Base.Pretty


-- Name -----------------------------------------------------------------------
-- | Names of variables.
data Name
        = NameGlobal String
        | NameLocal  String
        deriving (Eq, Show)

instance Pretty Name where
 ppr (NameGlobal str)   = text "@" <> text str
 ppr (NameLocal  str)   = text "%" <> text str


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


-- Exp ------------------------------------------------------------------------
data Exp 
        -- | Use of a variable.
        = XVar Var

        -- | A literal.
        | XLit Lit
        deriving (Eq, Show)  


instance Pretty Exp where
 ppr xx
  = case xx of
        XVar v  -> ppr v
        XLit l  -> ppr l


-- | Pretty print an expression without its type.
pprPlainX :: Exp -> Doc
pprPlainX xx
 = case xx of
        XVar v  -> ppr $ nameOfVar v
        XLit l  -> pprPlainL l


-- | Take the type of an expression.
typeOfExp :: Exp -> Type 
typeOfExp xx
 = case xx of
        XVar var        -> typeOfVar var
        XLit lit        -> typeOfLit lit
