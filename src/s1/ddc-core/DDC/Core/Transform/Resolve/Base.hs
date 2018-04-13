
module DDC.Core.Transform.Resolve.Base
        ( module DDC.Core.Env.EnvT
        , module DDC.Core.Module
        , module DDC.Core.Exp
        , module DDC.Type.Exp.Simple.Equiv
        , module Control.Monad.Trans.Except

        , S
        , Error (..))
where
import DDC.Core.Env.EnvT
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Exp.Simple.Equiv
import DDC.Data.Pretty                  hiding ((<$>))
import Control.Monad.Trans.Except


---------------------------------------------------------------------------------------------------
-- | Monad used during resolution.
--     We need IO so that we can search external interface during elaboration.
type S a n b = ExceptT (Error a n) IO b


---------------------------------------------------------------------------------------------------
data Error a n
        = ErrorCannotResolve (Type n)

instance (Eq n, Pretty n) => Pretty (Error a n) where
 ppr (ErrorCannotResolve tWanted)
  = vcat
  [ text "Cannot resolve elaboration"
  , text " of type: " % ppr tWanted ]

