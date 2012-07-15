
-- | Common simplifier recipies that combine multiple transforms.
module DDC.Core.Simplifier.Recipie
        (anormalize)
where
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.Namify
import DDC.Type.Env
import Data.Monoid

-- | Conversion to administrative normal-form.
anormalize 
        :: (Env n -> Namifier s n)
        -> (Env n -> Namifier s n)
        -> Simplifier s a n

anormalize namK namT
        =  Trans Snip 
        <> Trans Flatten 
        <> Trans (Namify namK namT)


