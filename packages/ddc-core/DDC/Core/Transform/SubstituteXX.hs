
-- | Capture avoiding substitution of expressions in expressions.
--
--   If a binder would capture a variable then it is anonymized
--   to deBruijn form.
module DDC.Core.Transform.SubstituteXX
        ( SubstituteXX(..)
        , substituteXX
        , substituteXXs
        , substituteXArg
        , substituteXArgs)
where
import DDC.Core.Exp
import DDC.Core.Collect
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteTX
import DDC.Type.Transform.SubstituteT
import DDC.Type.Rewrite
import Data.Maybe
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set


-- | Wrapper for `substituteWithX` that determines the set of free names in the
--   expression being substituted, and starts with an empty binder stack.
substituteXX 
        :: (Ord n, SubstituteXX c)
        => Bind n -> Exp a n -> c a n -> c a n

substituteXX b xArg xx
 | Just u       <- takeSubstBoundOfBind b
 = substituteWithXX xArg
        ( Sub 
        { subBound      = u

          -- Rewrite level-1 binders that have the same name as any
          -- of the free variables in the expression to substitute, 
          -- or any level-1 binders that expression binds itself.
        , subConflict1  
                = Set.fromList
                $  (mapMaybe takeNameOfBound $ Set.toList $ freeT Env.empty xArg) 
                ++ (mapMaybe takeNameOfBind  $ fst $ collectBinds xArg)

          -- Rewrite level-0 binders that have the same name as any
          -- of the free variables in the expression to substitute.
        , subConflict0
                = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeX Env.empty xArg
                
        , subStack1     = BindStack [] [] 0 0
        , subStack0     = BindStack [] [] 0 0
        , subShadow0    = False })
        xx

 | otherwise    = xx


-- | Wrapper for `substituteX` to substitute multiple expressions.
substituteXXs 
        :: (Ord n, SubstituteXX c)
        => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXXs bts x
        = foldr (uncurry substituteXX) x bts


-- | Substitute the argument of an application into an expression.
--   Perform type substitution for an `XType` 
--    and witness substitution for an `XWitness`
substituteXArg 
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => Bind n -> Exp a n -> c a n -> c a n

substituteXArg b arg x
 = case arg of
        XType t         -> substituteTX  b t x
        XWitness w      -> substituteWX  b w x
        _               -> substituteXX  b arg x


-- | Wrapper for `substituteXArgs` to substitute multiple arguments.
substituteXArgs
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => [(Bind n, Exp a n)] -> c a n -> c a n

substituteXArgs bas x
        = foldr (uncurry substituteXArg) x bas


-------------------------------------------------------------------------------
class SubstituteXX (c :: * -> * -> *) where
 substituteWithXX 
        :: forall a n. Ord n 
        => Exp a n -> Sub n -> c a n -> c a n 


instance SubstituteXX Exp where 
 substituteWithXX xArg sub xx
  = let down    = substituteWithXX xArg
        into    = rewriteWith
    in case xx of
        XVar a u
         -> case substX xArg sub u of
                Left  u' -> XVar a u'
                Right x  -> x

        XCon{}           -> xx
        XApp a x1 x2     -> XApp a (down sub x1) (down sub x2)

        XLAM a b x
         -> let (sub1, b')      = bind1 sub b
                x'              = down  sub1 x
            in  XLAM a b' x'

        XLam a b x
         -> let (sub1, b')      = bind0 sub  b
                x'              = down  sub1 x
            in  XLam a b' x'

        XLet a (LLet m b x1) x2
         -> let m'              = into  sub  m
                x1'             = down  sub  x1
                (sub1, b')      = bind0 sub  b
                x2'             = down  sub1 x2
            in  XLet a (LLet m' b' x1') x2'

        XLet a (LRec bxs) x2
         -> let (bs, xs)        = unzip  bxs
                (sub1, bs')     = bind0s sub bs
                xs'             = map (down sub1) xs
                x2'             = down sub1 x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegions b bs) x2
         -> let (sub1, b')      = bind1s sub  b
                (sub2, bs')     = bind0s sub1 bs
                x2'             = down   sub2 x2
            in  XLet a (LLetRegions b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down sub x2)

        XCase a x1 alts -> XCase a  (down sub x1) (map (down sub) alts)
        XCast a cc x1   -> XCast a  (down sub cc) (down sub x1)
        XType t         -> XType    (into sub t)
        XWitness w      -> XWitness (into sub w)
 

instance SubstituteXX Alt where
 substituteWithXX xArg sub aa
  = let down = substituteWithXX xArg
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down sub xBody
        
        AAlt (PData uCon bs) x
         -> let (sub1, bs')     = bind0s sub bs
                x'              = down   sub1 x
            in  AAlt (PData uCon bs') x'


instance SubstituteXX Cast where
 substituteWithXX xArg sub cc
  = let down = substituteWithXX xArg
        into = rewriteWith
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (into sub eff)
        CastWeakenClosure xs    -> CastWeakenClosure (map (down sub) xs)
        CastPurify w            -> CastPurify (into sub w)
        CastForget w            -> CastForget (into sub w)


-- | Rewrite or substitute into an expression variable.
substX  :: Ord n => Exp a n -> Sub n -> Bound n 
        -> Either (Bound n) (Exp a n)

substX xArg sub u
  = case substBound (subStack0 sub) (subBound sub) u of
        Left  u'                -> Left u'
        Right n  
         | not $ subShadow0 sub -> Right (liftX n xArg)
         | otherwise            -> Left  u


