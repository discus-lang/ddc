
module DDC.Type.Check.Base
        (CheckM (..))
where
import qualified DDC.Control.Monad.Check as G
import DDC.Type.Check.Error


-- | The type checker monad.
type CheckM n   = G.CheckM () (Error n)

