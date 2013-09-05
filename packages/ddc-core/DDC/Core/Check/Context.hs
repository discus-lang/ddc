
module DDC.Core.Check.Context
        (Direction (..))
where
import DDC.Type.Exp


-- | Direction used for bidirectional type checking.
data Direction n
        -- | Check the type of an expression against this one.
        = Check (Type n)

        -- | Synthesise the type of the expression.
        | Synth
        deriving Show
