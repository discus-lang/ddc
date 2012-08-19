
-- | Float casts outwards.
module DDC.Core.Transform.Bubble
        (bubbleX)
where
import DDC.Core.Exp
import DDC.Core.Compounds


bubbleX :: Exp a n -> Exp a n
bubbleX x
 = let  (cs, x')        = bubble x
        Just a          = takeAnnotOfExp x'
   in   dropCasts a cs x'


-- | Float casts outwards.
class Bubble (c :: * -> * -> *) where
 bubble :: c a n -> ([Cast a n], c a n)


instance Bubble Exp where
 bubble xx
  = case xx of
        XVar{}  -> ([], xx)
        XCon{}  -> ([], xx)

        -- Drop casts before we leave lambda abstractions, because the
        -- function type depends on the effect and closure of the body.
        -- The cast could also reference the bound variable.
        XLAM a b x
         -> let (cs, x')        = bubble x
            in  ([], XLAM a b (dropCasts a cs x'))

        XLam a b x
         -> let (cs, x')        = bubble x
            in  ([], XLam a b (dropCasts a cs x'))

        XApp a x1 x2
         -> let (cs1, x1')      = bubble x1
                (cs2, x2')      = bubble x2
            in  (cs1 ++ cs2, XApp a x1' x2')

        -- TODO: check whether casts mention bound cars
        XLet a lts x2
         -> let (cs1, lts')     = bubble lts
                (cs2, x2')      = bubble x2
            in  ( cs1
                , XLet a lts' (dropCasts a cs2 x2'))

        XCase a x alts
         -> let (cs, x')        = bubble x
                (css, alts')    = unzip $ map bubble alts
            in  ( cs ++ concat css
                , XCase a x' alts')

        -- Strip of cast and pass it up.
        XCast _ c x
         -> let (cs, x')        = bubble x
            in  (c : cs, x')

        XType{}         -> ([], xx)
        XWitness{}      -> ([], xx)


instance Bubble Lets where
 bubble lts
  = case lts of

        -- Drop casts before we leave let bindings because the casts
        -- could contain the bound variable.
        -- TODO: check for this and shift acceptable bindings further outwards.
        LLet m b x
         -> let (cs, x')        = bubble x
                Just a          = takeAnnotOfExp x'
            in  ([], LLet m b (dropCasts a cs x'))

        LRec bxs
         -> let bubbleRec (b, x)
                 = let  (cs, x') = bubble x
                        Just a   = takeAnnotOfExp x'
                   in   (b, dropCasts a cs x')

                bxs'            = map bubbleRec bxs

            in  ([], LRec bxs')

        LLetRegion{}            -> ([], lts)
        LWithRegion{}           -> ([], lts)


instance Bubble Alt where

 -- Default patterns don't bind variables, 
 -- so there is no problem floating casts outwards.
 bubble (AAlt PDefault x)
  = let (cs, x') = bubble x
    in  (cs, AAlt PDefault x')

 -- Drop casts before we leave the alt because they could contain
 -- variables bound by the pattern.
 -- TODO: check for this and shift accepable bindings further outwards.
 bubble (AAlt p x)
  = let (cs, x') = bubble x
        Just a   = takeAnnotOfExp x'
    in  ([], AAlt p $ dropCasts a cs x')


dropCasts :: a -> [Cast a n] -> Exp a n -> Exp a n
dropCasts a cs x
        = foldr (XCast a) x cs

