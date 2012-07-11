
module DDC.Core.Transform.Inline
        (inline)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env


inline  :: Ord n
        => (n -> Maybe (Exp a n))
        -> Exp a n
        -> Exp a n

inline getTemplate xx
        = transformUpX (inline1 getTemplate) Env.empty Env.empty xx


inline1 :: Ord n 
        => (n -> Maybe (Exp a n))       -- ^ Fn to return inliner templates.
        -> Env n   -> Env n
        -> Exp a n -> Exp a n

inline1 getTemplate _ _ xx
 = case xx of
        XVar _ (UName n _)
         | Just xx'     <- getTemplate n
         -> xx'

        _ -> xx
