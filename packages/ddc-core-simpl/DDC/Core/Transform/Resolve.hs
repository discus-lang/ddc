
-- | Resolve elaborations in a module.
module DDC.Core.Transform.Resolve
        ( resolveModule
        , Error (..))
where
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Data.Pretty


-------------------------------------------------------------------------------
resolveModule 
        :: Ord n
        => Module a n -> Either (Error a n) (Module a n)

resolveModule mm
 = Right mm



-------------------------------------------------------------------------------
data Error a n
        = ErrorCannotResolve    (Type n)

instance Pretty (Error a n) where
 ppr (ErrorCannotResolve _)
  = text "Cannot resolve elaboration"