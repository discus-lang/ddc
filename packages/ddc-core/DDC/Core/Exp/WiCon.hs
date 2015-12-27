
module DDC.Core.Exp.WiCon
        ( WiCon  (..))
where
import DDC.Type.Exp
import DDC.Type.Sum     ()
import Control.DeepSeq


-- | Witness constructors.
data WiCon n
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(Bound n) !(Type n)
        deriving (Show, Eq)


-- NFData ---------------------------------------------------------------------
instance NFData n => NFData (WiCon n) where
 rnf wi
  = case wi of
        WiConBound   u t        -> rnf u `seq` rnf t




