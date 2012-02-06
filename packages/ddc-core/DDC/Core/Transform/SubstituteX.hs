
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
import DDC.Core.Collect.FreeT
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Core.Transform.SubstituteW
import DDC.Core.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Wrapper for `substituteWithX` that determines the set of free names in the
--   type being substituted, and starts with an empty binder stack.
substituteX 
        :: ( Ord n, SubstituteX c)
        => Bind n -> Exp a n -> c a n -> c a n
substituteX b x' xx
  | Just u      <- takeSubstBoundOfBind b
  = let -- Determine the free names in the type we're subsituting.
        -- We'll need to rename binders with the same names as these
        fnsX    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeX Env.empty x'

        fnsT    = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeT Env.empty x'

        stack           = BindStack [] [] 0 0
 
   in   substituteWithX u x' fnsT fnsX stack stack xx

  | otherwise = xx


-- | Wrapper for `substituteX` to substitute multiple expressions.
substituteXs 
        :: (Ord n, SubstituteX c)
        => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXs bts x
        = foldr (uncurry substituteX) x bts


-- | Substitute the argument of an application into an expression.
--   Performtype substitution for an `XType` 
--    and witness substitution for an `XWitness`
substituteXArg 
        :: (Ord n, SubstituteX c, SubstituteW (c a), SubstituteT (c a))
        => Bind n -> Exp a n -> c a n -> c a n

substituteXArg b arg x
 = case arg of
        XType t         -> substituteT b t x
        XWitness w      -> substituteW b w x
        _               -> substituteX b arg x


-- | Wrapper for `substituteXArgs` to substitute multiple arguments.
substituteXArgs
        :: (Ord n, SubstituteX c, SubstituteW (c a), SubstituteT (c a))
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
        => Bound n              -- ^ Bound variable that we're subsituting for..
        -> Exp a n              -- ^ Exp to substitute.
        -> Set n                -- ^ Names of free spec   vars in the argument.
        -> Set n                -- ^ Names of free valwit vars in the environment.
        -> BindStack n          -- ^ Bind stack for spec names.
        -> BindStack n          -- ^ Bind stack for valwit names.
        -> c a n -> c a n


instance SubstituteX Exp where 
 substituteWithX u x fnsT fnsX stackT stackX xx
  = let down    = substituteWithX u x fnsT fnsX stackT stackX
    in case xx of
        XVar a u'
         -> case substBound stackX u u' of
                Left u'' -> XVar a u''
                Right n  -> liftX n x

        XCon{}          -> xx

        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLAM a b xBody
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let (stackT', b')   = pushBind fnsT stackT b
                xBody'          = substituteWithX u x fnsT fnsX stackT' stackX xBody
            in  XLAM a b' xBody'

        XLam a b xBody
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let (stackX', b')   = pushBind fnsX stackX b
                xBody'          = substituteWithX u x fnsT fnsX stackT stackX' xBody
            in  XLam a b' xBody'

        XLet a (LLet m b x1) x2
         | namedBoundMatchesBind u b -> xx
         | otherwise
         -> let x1'             = down x1
                (stackX', b')   = pushBind fnsX stackX b
                x2'             = substituteWithX u x fnsT fnsX stackT stackX' x2
            in  XLet a (LLet m b' x1') x2'

        XLet a (LRec bxs) x2
         -> let (stackX', bs')  = pushBinds fnsX stackX $ map fst bxs
                xs'             = [ if namedBoundMatchesBind u b 
                                        then xRight
                                        else substituteWithX u x fnsT fnsX stackT stackX' xRight
                                  | (b, xRight) <- bxs ]

                x2'             = substituteWithX u x fnsT fnsX stackT stackX' x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         -> let (stackT', b')   = pushBind  fnsT stackT b
                (stackX', bs')  = pushBinds fnsX stackX bs
                x2'             = substituteWithX u x fnsT fnsX stackT' stackX' x2
            in  XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down x2)

        XCase a x1 alts
         -> XCase a (down x1) 
                    (map (substituteWithX u x fnsT fnsX stackT stackX) alts)

        XCast a cc x1   
         -> XCast a cc (down x1)

        XType{}         -> xx
        XWitness{}      -> xx
 

instance SubstituteX Alt where
 substituteWithX u x fnsT fnsX stackT stackX aa
  = let down = substituteWithX u x fnsT fnsX stackT stackX
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down xBody
        
        AAlt (PData uCon bs) xBody
         | any (namedBoundMatchesBind u) bs -> aa
         | otherwise
         -> let (stackX', bs')  = pushBinds fnsX stackX bs
                xBody'          = substituteWithX u x fnsT fnsX stackT stackX' xBody
            in  AAlt (PData uCon bs') xBody'

