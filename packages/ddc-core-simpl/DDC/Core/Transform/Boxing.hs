
-- Wrap numeric literals 
--      23# 
--   => convert# [B# Nat#] [Nat#] 23#
--
-- Use unboxed versions of primitive operators.
--      add# [Nat#] x y 
--   => convert# [B# Nat#] [U# Nat#] 
--               (add# [U# Nat#] (convert# [U# Nat#] [B# Nat#] x)
--                               (convert# [U# Nat#] [B# Nat#] y))
--
-- NOTE: We do not allow unboxed types in the source program because we don't
-- want to deal with partial applications of functions to unboxed values.
--
-- With out setup we always have a version of each function that accepts
-- boxed values, so never need to do generic application involving
-- unboxed values. Fast-path function specialisations that take unboxed
-- parameters should be created separately, and not replace the existing
-- slow-path version.
--
-- TODO: Base local unboxing transform around analysis of convert
-- primop. 
--
module DDC.Core.Transform.Boxing
        ( Config        (..))
where
import DDC.Core.Exp


-- | Representation of the values of some type.
data Rep
        -- | Values of this type cannot be directly represented in the target
        --   language. We need to use a boxed or unboxed representation instead.
        = RepNone

        -- | Type is represented in boxed form,
        --   and thus can instantiate polymorphic types.
        | RepBoxed      

        -- | Type is represented in unboxed form,
        --   and thus cannot instantiate polymorphic types.
        | RepUnboxed
        deriving (Eq, Ord, Show)


data Config n
        = Config
        { -- | Get the representation of some type.
          configTypeRep         :: Type n -> Rep

          -- | Get the boxed version of some type.
        , configTypeBoxed       :: Type n -> Maybe (Type n) 

          -- | Get the unboxed version of some type.
        , configTypeUnboxed     :: Type n -> Maybe (Type n) }


class Boxing (c :: * -> * -> *) where
 boxing :: Ord n
        => Config n
        -> c a n
        -> c a n


instance Boxing Exp where
 boxing config xx
  = let down = boxing config
    in case xx of
        XVar{}  -> xx
        XCon{}  -> xx

        XLAM a b x
         -> let x'      = down x
            in  XLAM a b x'

        XLam a b x
         -> let x'      = down x
            in  XLam a b x'

        XApp a x1 x2
         -> let x1'     = down x1
                x2'     = down x2
            in  XApp a x1' x2'

        XLet a lts x
         -> let lts'    = down lts
                x'      = down x
            in  XLet a lts' x'

        XCase a x alts
         -> let x'      = down x
                alts'   = map down alts
            in  XCase a x' alts'

        XCast a c x
         -> let x'      = down x
            in  XCast a c x'

        XType{}         -> xx
        XWitness{}      -> xx


instance Boxing Lets where
 boxing config lts
  = let down    = boxing config
    in case lts of
        LLet b x
         -> let x'      = down x
            in  LLet b x'

        LRec bxs
         -> let bxs'    = [(b, down x) | (b, x) <- bxs]
            in  LRec bxs'

        LPrivate{}      -> lts
        LWithRegion{}   -> lts


instance Boxing Alt where
 boxing config alt
  = case alt of
        AAlt w x
         -> let x'      = boxing config x
            in  AAlt w x'
