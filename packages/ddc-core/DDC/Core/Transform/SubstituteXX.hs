
-- | Expression substitution.
module DDC.Core.Transform.SubstituteXX
        ( SubstituteXX(..)
        , substituteXX
        , substituteXXs
        , substituteXArg
        , substituteXArgs)
where
import DDC.Core.Exp
import DDC.Core.Collect.FreeX
import DDC.Core.Collect.FreeT
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteTX
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Wrapper for `substituteWithX` that determines the set of free names in the
--   expression being substituted, and starts with an empty binder stack.
substituteXX 
        :: (Ord n, SubstituteXX c)
        => Bind n -> Exp a n -> c a n -> c a n

substituteXX b x' xx
  | Just u      <- takeSubstBoundOfBind b
  = let -- Determine the free names in the expression we're subsituting.
        -- We'll need to rename binders with the same names as these
        fnsX    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeX Env.empty x'

        fnsT    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeT Env.empty x'

        stack   = BindStack [] [] 0 0
 
   in   substituteWithXX u x' fnsT fnsX stack stack xx

  | otherwise = xx


-- | Wrapper for `substituteX` to substitute multiple expressions.
substituteXXs 
        :: (Ord n, SubstituteXX c)
        => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXXs bts x
        = foldr (uncurry substituteXX) x bts


-- | Substitute the argument of an application into an expression.
--   Performtype substitution for an `XType` 
--    and witness substitution for an `XWitness`
substituteXArg 
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => Bind n -> Exp a n -> c a n -> c a n

substituteXArg b arg x
 = case arg of
        XType t         -> substituteTX b t x
        XWitness w      -> substituteWX  b w x
        _               -> substituteXX  b arg x


-- | Wrapper for `substituteXArgs` to substitute multiple arguments.
substituteXArgs
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => [(Bind n, Exp a n)] -> c a n -> c a n

substituteXArgs bas x
        = foldr (uncurry substituteXArg) x bas


-- SubstituteX ----------------------------------------------------------------
class SubstituteXX (c :: * -> * -> *) where

 -- | Substitute a type into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithXX
        :: forall a n. Ord n
        => Bound n      -- ^ Bound variable that we're subsituting for..
        -> Exp a n      -- ^ Exp to substitute.
        -> Set n        -- ^ Names of free spec   vars in the argument.
        -> Set n        -- ^ Names of free valwit vars in the environment.
        -> BindStack n  -- ^ Bind stack for spec names.
        -> BindStack n  -- ^ Bind stack for valwit names.
        -> c a n -> c a n


instance SubstituteXX Exp where 
 substituteWithXX u x fnsT fnsX stackT stackX xx
  = let down    = substituteWithXX u x fnsT fnsX stackT stackX
    in case xx of
        XVar a u'
         -> case substBound stackX u u' of
                Left u'' -> XVar a u''
                Right n  -> liftX n x

        XCon{}          -> xx

        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLAM a b xBody
         -> let (stackT', b')   = pushBind fnsT stackT b
                xBody'          = substituteWithXX u x fnsT fnsX stackT' stackX xBody
            in  XLAM a b' xBody'

        XLam a b xBody
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let (stackX', b')   = pushBind fnsX stackX b
                xBody'          = substituteWithXX u x fnsT fnsX stackT stackX' xBody
            in  XLam a b' xBody'

        XLet a (LLet m b x1) x2
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let (stackX', b')   = pushBind fnsX stackX b
                x1'             = down x1
                x2'             = substituteWithXX u x fnsT fnsX stackT stackX' x2
            in  XLet a (LLet m b' x1') x2'

        XLet a (LRec bxs) x2
         | any (namedBoundMatchesBind u) $ map fst bxs -> xx
         | otherwise
         -> let (bs, xs)        = unzip bxs
                (stackX', bs')  = pushBinds fnsX stackX bs
                xs'             = map (substituteWithXX u x fnsT fnsX stackT stackX') xs
                x2'             = substituteWithXX u x fnsT fnsX stackT stackX' x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         | any (namedBoundMatchesBind u) bs -> xx
         | otherwise
         -> let (stackT', b')   = pushBind  fnsT stackT b
                (stackX', bs')  = pushBinds fnsX stackX bs
                x2'             = substituteWithXX u x fnsT fnsX stackT' stackX' x2
            in  XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down x2)

        XCase a x1 alts -> XCase a    (down x1) (map down alts)
        XCast a cc x1   -> XCast a cc (down x1)
        XType{}         -> xx
        XWitness{}      -> xx
 

instance SubstituteXX Alt where
 substituteWithXX u x fnsT fnsX stackT stackX aa
  = let down = substituteWithXX u x fnsT fnsX stackT stackX
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down xBody
        
        AAlt (PData uCon bs) xBody
         | any (namedBoundMatchesBind u) bs -> aa
         | otherwise
         -> let (stackX', bs')  = pushBinds fnsX stackX bs
                xBody'          = substituteWithXX u x fnsT fnsX stackT stackX' xBody
            in  AAlt (PData uCon bs') xBody'

