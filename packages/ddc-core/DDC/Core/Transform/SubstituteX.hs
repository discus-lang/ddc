
-- | Type substitution.
module DDC.Core.Transform.SubstituteX
        ( SubstituteX(..))
where
import DDC.Core.Exp
import DDC.Core.Collect.Free
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


class SubstituteX (c :: * -> * -> *) where
 -- | Substitute a type into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithX
        :: forall a n. Ord n
        => Bound n              -- ^ Bound variable that we're subsituting into.
        -> Exp a n              -- ^ Exp to substituteX.
        -> Set  n               -- ^ Names of free varaibles in the exp to substitute.
        -> BindStack n          -- ^ Bind stack.
        -> c a n -> c a n

 -- | Wrapper for `substituteWithX` that determines the set of free names in the
 --   type being substituted, and starts with an empty binder stack.
 substituteX :: (SubstituteX c, Ord n) => Bound n -> Exp a n -> c a n -> c a n
 substituteX u t x
  = let -- Determine the free names in the type we're subsituting.
        -- We'll need to rename binders with the same names as these
        freeNames       = Set.fromList
                        $ mapMaybe takeNameOfBound 
                        $ Set.toList 
                        $ free Env.empty t

        stack           = BindStack [] 0 0
 
   in   substituteWithX u t freeNames stack x


instance SubstituteX Exp where 
 substituteWithX u x fns stack xx
  = let down    = substituteWithX u x fns stack 
    in case xx of
        XVar a u'
         -> case substBound stack u u' of
                Left u'' -> XVar a u''
                Right n  -> liftX n x
                
        XCon{}          -> xx
        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLet a (LLet b x1) x2
         -> let x1'             = substituteWithX u x fns stack x1
                (stack', b')    = pushBind fns stack b
                x2'             = substituteWithX u x fns stack' x2
            in  XLet a (LLet b' x1') x2'

        XType{}         -> xx
        XWitness{}      -> xx

        _       -> error "substituteWithX: not done yet"
 
 
 







