
-- | Common simplifier recipies that combine multiple transforms.
module DDC.Core.Simplifier.Recipie
        (anormalize)
where
import DDC.Core.Simplifier.Base
import Data.Monoid

-- | Conversion to administrative normal-form.
anormalize :: Simplifier
anormalize = Trans Snip <> Trans Flatten <> Trans Namify


