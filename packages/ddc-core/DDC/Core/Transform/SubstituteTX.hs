
-- | Capture avoiding substitution of types in expressions. 
--
--   If a binder would capture a variable then it is anonymized
--   to deBruijn form.
--
module DDC.Core.Transform.SubstituteTX
        ( SubstituteTX(..)
        , substituteTX
        , substituteTXs
        , substituteBoundTX)
where
import DDC.Core.Collect
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Rename
import Data.Maybe
import qualified Data.Set               as Set
import qualified DDC.Type.Env           as Env


-- | Substitute a `Type` for the `Bound` corresponding to some `Bind` in a thing.
substituteTX :: (SubstituteTX c, Ord n) => Bind n -> Type n -> c n -> c n
substituteTX b t x
 = case takeSubstBoundOfBind b of
    Just u      -> substituteBoundTX u t x
    _           -> x


-- | Wrapper for `substituteT` to substitute multiple types.
substituteTXs :: (SubstituteTX c, Ord n) => [(Bind n, Type n)] -> c n -> c n
substituteTXs bts x
        = foldr (uncurry substituteTX) x bts


-- | Substitute a `Type` for a `Bound` in some thing.
substituteBoundTX :: (SubstituteTX c, Ord n) => Bound n -> Type n -> c n -> c n
substituteBoundTX u tArg xx
 = substituteWithTX tArg
        ( Sub 
        { subBound      = u

          -- Rewrite level-1 binders that have the same name as any
          -- of the free variables in the expression to substitute.
        , subConflict1  
                = Set.fromList
                $  (mapMaybe takeNameOfBound $ Set.toList $ freeT Env.empty tArg) 

          -- Rewrite level-0 binders that have the same name as any
          -- of the free variables in the expression to substitute.
        , subConflict0  = Set.empty
        , subStack1     = BindStack [] [] 0 0
        , subStack0     = BindStack [] [] 0 0
        , subShadow0    = False })
        xx


-------------------------------------------------------------------------------
class SubstituteTX (c :: * -> *) where
 substituteWithTX
        :: forall n. Ord n
        => Type n -> Sub n -> c n -> c n


instance SubstituteTX (Exp a) where 
 substituteWithTX tArg sub xx
  = {-# SCC substituteWithTX #-}
    let down x   = substituteWithTX tArg x
    in case xx of
        XVar a u        -> XVar a u
        XCon{}          -> xx
        XApp a x1 x2    -> XApp a (down sub x1) (down sub x2)

        XLAM a b x
         -> let (sub1, b')      = bind1 sub b
                x'              = down  sub1 x
            in  XLAM a b' x'

        XLam a b x
         -> let (sub1, b')      = bind0 sub  (down sub b)
                x'              = down  sub1 x
            in  XLam a b' x'

        XLet a (LLet b x1) x2
         -> let x1'             = down  sub  x1
                (sub1, b')      = bind0 sub  (down sub b)
                x2'             = down  sub1 x2
            in  XLet a (LLet b' x1') x2'

        XLet a (LRec bxs) x2
         -> let (bs, xs)        = unzip  bxs
                (sub1, bs')     = bind0s sub (map (down sub) bs)
                xs'             = map (down sub1) xs
                x2'             = down sub1 x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegions b bs) x2
         -> let (sub1, b')      = bind1s sub  b
                (sub2, bs')     = bind0s sub1 (map (down sub1) bs)
                x2'             = down   sub2 x2
            in  XLet a (LLetRegions b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down sub x2)

        XCase a x1 alts -> XCase a  (down sub x1) (map (down sub) alts)
        XCast a cc x1   -> XCast a  (down sub cc) (down sub x1)
        XType t         -> XType    (down sub t)
        XWitness w      -> XWitness (down sub w)


instance SubstituteTX (Alt a) where
 substituteWithTX tArg sub aa
  = let down x = substituteWithTX tArg x
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down sub xBody
        
        AAlt (PData uCon bs) x
         -> let (sub1, bs')     = bind0s sub (map (down sub) bs)
                x'              = down   sub1 x
            in  AAlt (PData uCon bs') x'


instance SubstituteTX (Cast a) where
 substituteWithTX tArg sub cc
  = let down x   = substituteWithTX tArg x
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (down sub eff)
        CastWeakenClosure clo   -> CastWeakenClosure (map (down sub) clo)
        CastPurify w            -> CastPurify        (down sub w)
        CastForget w            -> CastForget        (down sub w)
        CastSuspend             -> CastSuspend
        CastRun                 -> CastRun


instance SubstituteTX (Witness a) where
 substituteWithTX tArg sub ww
  = let down x   = substituteWithTX tArg x
    in case ww of
        WVar  a u               -> WVar  a u
        WCon{}                  -> ww
        WApp  a w1 w2           -> WApp  a (down sub w1) (down sub w2)
        WJoin a w1 w2           -> WJoin a (down sub w1) (down sub w2)
        WType a t               -> WType a (down sub t)


instance SubstituteTX Bind where
 substituteWithTX tArg sub bb
  = replaceTypeOfBind (substituteWithTX tArg sub (typeOfBind bb))  bb


instance SubstituteTX Type where
 substituteWithTX tArg sub tt
        = substituteWithT
                (subBound sub)
                tArg
                (subConflict1 sub)
                (subStack1    sub)
                tt
