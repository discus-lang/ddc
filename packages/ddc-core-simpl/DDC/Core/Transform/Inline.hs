
-- | Inlining definitions into their use sites.
module DDC.Core.Transform.Inline
        ( inline
        , InlineSpec   (..)
        , lookupTemplateFromModules)
where
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Transform.Inline.Templates
import qualified Data.Set               as Set
import Data.Set                         (Set)


class Inline (c :: * -> * -> *) where
 inline :: Ord n
        => (n -> Maybe (Exp a n))
                        -- ^ Get the template for a named variable.
        -> Set n        -- ^ Don't inline definitions for these names.
        -> c a n
        -> c a n


instance Inline Module where
 inline get inside mm
  = mm  { moduleBody = inline get inside (moduleBody mm) }


instance Inline Exp where
 inline get inside xx
  = let down x = inline get inside x
    in case xx of
        XVar _ (UName n)
         -- Don't inline a recursive definition into itself.
         | Set.member n inside
         -> xx

         -- If there is a template for this variable then inline it, 
         -- but remember that we're now inside the body so we don't inline
         -- recursive functions forever.
         | Just xx'     <- get n
         -> let !inside' = Set.insert n inside
            in  inline get inside' xx'

        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b (down x)
        XLam  a b x     -> XLam  a b (down x)
        XApp  a x1 x2   -> XApp  a (down x1)  (down x2)
        XLet  a lts x2  -> XLet  a (down lts) (down x2)
        XCase a x alts  -> XCase a (down x)   (map down alts)
        XCast a c x     -> XCast a c          (down x)
        XType{}         -> xx
        XWitness{}      -> xx


instance Inline Lets where
 inline get inside lts
  = let enter b x
         = case b of
                BName n _       -> inline get (Set.insert n inside) x
                _               -> inline get inside x

    in case lts of
        LLet b x        -> LLet b (enter b x)
        LRec bxs        -> LRec [(b, enter b x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


instance Inline Alt where
 inline get inside alt
  = case alt of
        AAlt p x        -> AAlt p (inline get inside x)


