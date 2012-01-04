
-- | Type substitution.
module DDC.Core.Transform.SubstituteW
        ( SubstituteW(..)
        , substituteW
        , substituteWs)
where
import DDC.Core.Exp
import DDC.Core.Collect.Free
-- import DDC.Core.Transform.LiftW
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Wrapper for `substituteWithW` that determines the set of free names in the
--   type being substituted, and starts with an empty binder stack.
substituteW :: (SubstituteW c, Ord n) => Bind n -> Witness n -> c n -> c n
substituteW b w x
 | Just u       <- takeSubstBoundOfBind b
 = let -- Determine the free names in the type we're subsituting.
       -- We'll need to rename binders with the same names as these
       freeNames       = Set.fromList
                       $ mapMaybe takeNameOfBound 
                       $ Set.toList 
                       $ free Env.empty w

       stack           = BindStack [] [] 0 0
 
  in   substituteWithW u w freeNames stack x

 | otherwise    = x
 

-- | Wrapper for `substituteW` to substitute multiple things.
substituteWs :: (SubstituteW c, Ord n) => [(Bind n, Witness n)] -> c n -> c n
substituteWs bts x
        = foldr (uncurry substituteW) x bts


class SubstituteW (c :: * -> *) where
 -- | Substitute a witness into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithW
        :: forall n. Ord n
        => Bound n              -- ^ Bound variable that we're subsituting into.
        -> Witness n            -- ^ Witness to substitute.
        -> Set  n               -- ^ Names of free varaibles in the exp to substitute.
        -> BindStack n          -- ^ Bind stack.
        -> c n -> c n


-- Instances --------------------------------------------------------------------------------------
instance SubstituteW Witness where
 substituteWithW u w fns stack ww
  = let down    = substituteWithW u w fns stack
    in case ww of
        WCon{}                  -> ww
        WApp  w1 w2             -> WApp  (down w1) (down w2)
        WJoin w1 w2             -> WJoin (down w1) (down w2)
        WType{}                 -> ww

        WVar u'
         -> case substBound stack u u' of
                Left u''  -> WVar u''
                Right _n  -> w          -- TODO: liftW by n


instance SubstituteW (Exp a) where 
 substituteWithW u w fns stack xx
  = let down    = substituteWithW u w fns stack 
    in case xx of
        XVar{}                  -> xx
        XCon{}                  -> xx
        XApp a x1 x2            -> XApp a (down x1) (down x2)
        XLam a b x              -> XLam a b (down x)
        XLet a (LLet b x1) x2   -> XLet a (LLet b (down x1)) (down x2)
        XLet{}                  -> error "substituetWithW: XLet not done yet"
        XCase{}                 -> error "substituteWithW: XCase not done yet"
        XCast a c x             -> XCast a (down c) (down x)
        XType{}                 -> xx
        XWitness w1             -> XWitness (down w1)


instance SubstituteW Cast where
 substituteWithW u w fns stack cc
  = let down    = substituteWithW u w fns stack 
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  eff
        CastWeakenClosure clo   -> CastWeakenClosure clo
        CastPurify w'           -> CastPurify (down w')
        CastForget w'           -> CastForget (down w')
