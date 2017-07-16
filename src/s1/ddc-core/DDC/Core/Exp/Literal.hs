
module DDC.Core.Exp.Literal
        ( Literal       (..))
where
import Data.Text        (Text)


-- | Types of literal values known to the compiler.
--
--   Note that literals are embedded in the name type of each fragment
--   rather than in the expression itself so that fragments can
--   choose which types of literals they support.
--
data Literal
        = LInt    Integer
        | LNat    Integer
        | LSize   Integer
        | LWord   Integer Int
        | LFloat  Double  (Maybe Int)
        | LChar   Char
        | LString Text
        deriving (Eq, Show)
