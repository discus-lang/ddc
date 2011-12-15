
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


-- | Wrapper for `substituteWithW` that determines the set of free names in the
--   type being substituted, and starts with an empty binder stack.
substituteW :: (SubstituteW c, Ord n) => Bound n -> Witness n -> c n -> c n
substituteW u w x
 = let -- Determine the free names in the type we're subsituting.
       -- We'll need to rename binders with the same names as these
       freeNames       = Set.fromList
                       $ mapMaybe takeNameOfBound 
                       $ Set.toList 
                       $ free Env.empty w

       stack           = BindStack [] 0 0
 
  in   substituteWithW u w freeNames stack x


-- | Wrapper for `substituteW` to substitute multiple things.
substituteWs :: (SubstituteW c, Ord n) => [(Bound n, Witness n)] -> c n -> c n
substituteWs bts x
        = foldr (uncurry substituteW) x bts


-- Instances --------------------------------------------------------------------------------------
instance SubstituteW (Exp a) where 
 substituteWithW u w fns stack xx
  = let down    = substituteWithW u w fns stack 
    in case xx of
        XVar{}                  -> xx
        XCon{}                  -> xx
        XApp a x1 x2            -> XApp a (down x1) (down x2)
        XLet a (LLet b x1) x2   -> XLet a (LLet b (down x1)) (down x2)
        XType{}                 -> xx
        XWitness w1             -> XWitness (down w1)

        _       -> error "substituteWithW: not done yet"
 

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

