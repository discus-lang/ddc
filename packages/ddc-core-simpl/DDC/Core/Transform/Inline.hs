
module DDC.Core.Transform.Inline
        (inline)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX
import Data.Functor.Identity


inline  :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (n -> Maybe (Exp a n))
        -> c a n
        -> c a n

inline getTemplate xx
        = transformUpX' (inline1 getTemplate) xx


inline1 :: Ord n 
        => (n -> Maybe (Exp a n))       -- ^ Fn to return inliner templates.
        -> Exp a n -> Exp a n

inline1 getTemplate xx
 = case xx of
        XVar _ (UName n)
         | Just xx'     <- getTemplate n
         -> xx'

        _ -> xx
