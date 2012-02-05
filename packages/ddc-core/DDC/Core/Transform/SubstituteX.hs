
-- | Type substitution.
module DDC.Core.Transform.SubstituteX
        ( SubstituteX(..)
        , substituteX
        , substituteXs
        , substituteXArg
        , substituteXArgs)
where
import DDC.Core.Exp
import DDC.Core.Collect.FreeX
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Core.Transform.SubstituteW
import DDC.Core.Transform.SubstituteT
import DDC.Base.Pretty          (Pretty)
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)



-- | Wrapper for `substituteWithX` that determines the set of free names in the
--   type being substituted, and starts with an empty binder stack.
substituteX :: (SubstituteX c, Ord n, Pretty n) => Bind n -> Exp a n -> c a n -> c a n
substituteX b t x
  | Just u      <- takeSubstBoundOfBind b
  = let -- Determine the free names in the type we're subsituting.
        -- We'll need to rename binders with the same names as these
        freeNames       = Set.fromList
                        $ mapMaybe takeNameOfBound 
                        $ Set.toList 
                        $ freeX Env.empty t                             -- TODO: type vars may be captured by type binders

        stack           = BindStack [] [] 0 0
 
   in   substituteWithX u t freeNames stack x

  | otherwise = x


-- | Wrapper for `substituteX` to substitute multiple expressions.
substituteXs :: (SubstituteX c, Ord n, Pretty n) => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXs bts x
        = foldr (uncurry substituteX) x bts


-- | Substitute the argument of an application into an expression.
--   Performtype substitution for an `XType` 
--    and witness substitution for an `XWitness`
substituteXArg 
        :: (Ord n, Pretty n, SubstituteX c, SubstituteW (c a), SubstituteT (c a))
        => Bind n -> Exp a n -> c a n -> c a n

substituteXArg b arg x
 = case arg of
        XType t         -> substituteT b t x
        XWitness w      -> substituteW b w x
        _               -> substituteX b arg x


-- | Wrapper for `substituteXArgs` to substitute multiple arguments.
substituteXArgs
        :: (Ord n, Pretty n, SubstituteX c, SubstituteW (c a), SubstituteT (c a))
        => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXArgs bas x
        = foldr (uncurry substituteXArg) x bas


-- SubstituteX ----------------------------------------------------------------
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

        XLAM a b xBody  -> XLAM a b (down xBody)                               -- TODO: handle var capture on lambda
                                                                                -- push bind on kenv

        XLam a b xBody
         -> let (stack', b')    = pushBind fns stack b
                xBody'          = substituteWithX u x fns stack' xBody
            in  XLam a b' xBody'

        XLet a (LLet m b x1) x2
         -> let x1'             = down x1
                (stack', b')    = pushBind fns stack b
                x2'             = substituteWithX u x fns stack' x2
            in  XLet a (LLet m b' x1') x2'

        XLet a (LRec bxs) x2
         -> let (bs, xs)        = unzip bxs
                (stack', bs')   = pushBinds fns stack bs
                xs'             = map (substituteWithX u x fns stack') xs
                x2'             = substituteWithX u x fns stack' x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         -> let (stack1, b')    = pushBind  fns stack  b
                (stack2, bs')   = pushBinds fns stack1 bs
                x2'             = substituteWithX u x fns stack2 x2
            in  XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion uR) x2
         ->     XLet a (LWithRegion uR) (down x2)

        XCase a x1 alts
         -> let x1'             = substituteWithX u x fns stack x1
                alts'           = map (substituteWithX u x fns stack) alts
            in  XCase a x1' alts'

        XCast a cc x1   -> XCast a cc (down x1)

        XType{}         -> xx
        XWitness{}      -> xx
 

instance SubstituteX Alt where
 substituteWithX u x fns stack aa
  = let down = substituteWithX u x fns stack
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down xBody
        
        AAlt (PData uCon bs) xBody
         -> let (stack', bs')   = pushBinds fns stack bs
                xBody'          = substituteWithX u x fns stack' xBody
            in  AAlt (PData uCon bs') xBody'

