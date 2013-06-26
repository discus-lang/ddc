
module DDC.Core.Exp.Pat
        ( Pat (..))
where
import DDC.Core.Exp.DaCon
import DDC.Type.Exp
import Control.DeepSeq


-- | Pattern matching.
data Pat n
        -- | The default pattern always succeeds.
        = PDefault
        
        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon n) ![Bind n]
        deriving (Show, Eq)
        

instance NFData n => NFData (Pat n) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PData dc bs             -> rnf dc `seq` rnf bs


