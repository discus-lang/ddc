
module DDC.Llvm.Syntax.Exp
        ( -- * Expressions
          Exp   (..)
        , typeOfExp

          -- * Variables
        , Var   (..)
        , nameOfVar
        , typeOfVar

          -- * Names
        , Name  (..)

          -- * Literals
        , Lit   (..)
        , typeOfLit)
where
import DDC.Llvm.Syntax.Type
import Data.ByteString                  (ByteString)
import qualified Data.ByteString        as BS


-- Exp ------------------------------------------------------------------------
data Exp 
        -- | Use of a variable.
        = XVar   Var

        -- | A literal.
        | XLit   Lit

        -- | An undefined value.
        | XUndef Type
        deriving (Eq, Show)  


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


instance Ord Var where
 compare (Var n1 _) (Var n2 _)
        = compare n1 n2


-- Name -----------------------------------------------------------------------
-- | Names of variables.
data Name
        = NameGlobal String
        | NameLocal  String
        deriving (Show, Eq, Ord)


-- Lit ------------------------------------------------------------------------
-- | Literal data.
data Lit
        -- | An integer literal
        = LitInt        Type    Integer

        -- | A floating-point literal.
        | LitFloat      Type    Double

        -- | A string literal.
        --   In LLVM these have the same type as array literals, but have a
        --   special syntax.
        | LitString     ByteString

        -- | A null pointer literal.
        --   Only applicable to pointer types
        | LitNull       Type

        -- | A completely undefined value.
        | LitUndef      Type
        deriving (Eq, Show)


-- | Yield the `Type` of a `Lit`.
typeOfLit :: Lit -> Type
typeOfLit ll
 = case ll of
        LitInt    t _   -> t
        LitFloat  t _   -> t
        LitString bs    -> TArray (fromIntegral $ BS.length bs) (TInt 8)
        LitNull   t     -> t
        LitUndef  t     -> t

