
-- | Type substitution.
module DDC.Core.Transform.SubstituteWX
        ( SubstituteWX(..)
        , substituteWX
        , substituteWXs)
where
import DDC.Core.Exp
import DDC.Core.Collect.FreeX
import DDC.Core.Collect.FreeT
import DDC.Core.Transform.LiftW
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Wrapper for `substituteWithW` that determines the set of free names in the
--   type being substituted, and starts with an empty binder stack.
substituteWX :: (SubstituteWX c, Ord n) => Bind n -> Witness n -> c n -> c n
substituteWX b w xx
 | Just u       <- takeSubstBoundOfBind b
 = let  -- Determine the free names in the type we're subsituting.
        -- We'll need to rename binders with the same names as these
        fnsX    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeX Env.empty w

        fnsT    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeT Env.empty w
       
        stack    = BindStack [] [] 0 0
 
  in    substituteWithWX u w fnsT fnsX stack stack xx

 | otherwise    = xx
 

-- | Wrapper for `substituteW` to substitute multiple things.
substituteWXs :: (SubstituteWX c, Ord n) => [(Bind n, Witness n)] -> c n -> c n
substituteWXs bts x
        = foldr (uncurry substituteWX) x bts


class SubstituteWX (c :: * -> *) where
 -- | Substitute a witness into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithWX
        :: forall n. Ord n
        => Bound n      -- ^ Bound variable that we're subsituting into.
        -> Witness n    -- ^ Witness to substitute.
        -> Set n        -- ^ Names of free spec names in the exp to substitute.
        -> Set n        -- ^ Names of free valwit names in the exp to substitute.
        -> BindStack n  -- ^ Bind stack for spec names.
        -> BindStack n  -- ^ Bind stack for valwit names.
        -> c n -> c n


-- Instances ------------------------------------------------------------------
instance SubstituteWX Witness where
 substituteWithWX u w fnsT fnsX stackT stackX ww
  = let down    = substituteWithWX u w fnsT fnsX stackT stackX
    in case ww of
        WVar u'
         -> case substBound stackX u u' of
                Left u'' -> WVar u''
                Right n  -> liftW n w

        WCon{}                  -> ww
        WApp  w1 w2             -> WApp  (down w1) (down w2)
        WJoin w1 w2             -> WJoin (down w1) (down w2)
        WType{}                 -> ww


instance SubstituteWX (Exp a) where 
 substituteWithWX u w fnsT fnsX stackT stackX xx
  = let down    = substituteWithWX u w fnsT fnsX stackT stackX
    in case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XApp  a x1 x2   -> XApp  a   (down x1)  (down x2)

        XLAM  a b xBody
         -> let (stackT', b')   = pushBind fnsX stackX b
                xBody'          = substituteWithWX u w fnsT fnsX stackT' stackX xBody
            in  XLAM  a b' xBody'

        XLam  a b xBody
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let (stackX', b')   = pushBind fnsX stackX b
                xBody'          = substituteWithWX u w fnsT fnsX stackT stackX' xBody
            in  XLam  a b' xBody'

        XLet  a (LLet m b x1) x2
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let m'              = down m
                (stackX', b')   = pushBind fnsX stackX b
                x1'             = down x1
                x2'             = substituteWithWX u w fnsT fnsX stackT stackX' x2
            in  XLet a (LLet m' b' x1') x2'

        XLet a (LRec bxs) x2
         | any (namedBoundMatchesBind u) $ map fst bxs -> xx
         | otherwise
         -> let (bs, xs)        = unzip bxs
                (stackX', bs')  = pushBinds fnsX stackX bs
                xs'             = map (substituteWithWX u w fnsT fnsX stackT stackX') xs
                x2'             = substituteWithWX u w fnsT fnsX stackT stackX' x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         | any (namedBoundMatchesBind u) bs -> xx
         | otherwise
         -> let (stackT', b')   = pushBind  fnsT stackT b
                (stackX', bs')  = pushBinds fnsX stackX bs
                x2'             = substituteWithWX u w fnsT fnsX stackT' stackX' x2
            in  XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down x2)

        XCase a x alts  -> XCase a  (down x)   (map down alts)
        XCast a c x     -> XCast a  (down c)   (down x)
        XType{}         -> xx
        XWitness w1     -> XWitness (down w1)


instance SubstituteWX LetMode where
 substituteWithWX u f fnsT fnsX stackT stackX lm
  = let down = substituteWithWX u f fnsT fnsX stackT stackX
    in case lm of
        LetStrict        -> lm
        LetLazy Nothing  -> LetLazy Nothing
        LetLazy (Just w) -> LetLazy (Just (down w))


instance SubstituteWX (Alt a) where
 substituteWithWX u w fnsT fnsX stackT stackX alt
  = let down    = substituteWithWX u w fnsT fnsX stackT stackX
    in case alt of
        AAlt PDefault xBody
         -> AAlt PDefault $ down xBody
        
        AAlt (PData uCon bs) xBody
         | any (namedBoundMatchesBind u) bs -> alt
         | otherwise
         -> let (stackX', bs')  = pushBinds fnsX stackX bs
                xBody'          = substituteWithWX u w fnsT fnsX stackT stackX' xBody
            in  AAlt (PData uCon bs') xBody'


instance SubstituteWX Cast where
 substituteWithWX u w fnsT fnsX stackT stackX cc
  = let down    = substituteWithWX u w fnsT fnsX stackT stackX 
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  eff
        CastWeakenClosure clo   -> CastWeakenClosure clo
        CastPurify w'           -> CastPurify (down w')
        CastForget w'           -> CastForget (down w')


