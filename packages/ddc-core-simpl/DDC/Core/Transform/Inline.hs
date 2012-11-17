
-- | Inlining definitions into their use sites.
module DDC.Core.Transform.Inline
        ( inline
        , lookupTemplateFromModules)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.Inline.Templates
import Data.Functor.Identity


-- | Inline the definitions of named bound variables into their use sites
--   in some core thing.
inline  :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (n -> Maybe (Exp a n))       -- ^ Lookup the inliner template for
                                        --   some name.
        -> c a n                        -- ^ Inline into this thing.
        -> c a n

inline getTemplate xx
        = transformUpX' (inline1 getTemplate) xx


inline1 :: Ord n 
        => (n -> Maybe (Exp a n))       -- ^ Lookup the inliner template for
                                        --   some name.
        -> Exp a n                      -- ^ Inline into this expression.
        -> Exp a n

inline1 getTemplate xx
 = case xx of
        XVar _ (UName n)
         | Just xx'     <- getTemplate n
         -> xx'

        _ -> xx
