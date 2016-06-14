
module DDC.Source.Tetra.Exp.Bind
        ( Bind  (..)
        , Bound (..))
where
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
