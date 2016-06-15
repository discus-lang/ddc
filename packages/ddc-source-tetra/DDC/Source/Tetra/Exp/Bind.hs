
module DDC.Source.Tetra.Exp.Bind
        ( Bind          (..)
        , Bound         (..)
        , DaConBind     (..)
        , DaConBound    (..)
        , TyConBind     (..)
        , TyConBound    (..))
where
import DDC.Source.Tetra.Prim.Base
import Data.Text                (Text)

-- | Binding occurrence of a variable.
data Bind
        = BNone
        | BAnon
        | BName !Text
        deriving Show


-- | Bound occurrence of a variable.
data Bound 
        = UIx   !Int
        | UName !Text
        deriving (Show, Eq)


-- | Binding occurrence of a data constructor.
data DaConBind
        = DaConBindName  Text
        deriving Show


-- | Bound occurrences of a data constructor.
data DaConBound
        = DaConBoundName Text
        | DaConBoundLit  PrimLit
        deriving Show


-- | Binding occurrence of a type constructor.
data TyConBind
        = TyConBindName  Text
        deriving Show


-- | Bound occurrence of a type constructor.
data TyConBound
        = TyConBoundName Text
        deriving Show
