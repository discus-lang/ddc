
-- | Type substitution.
module DDC.Core.Transform.SubstituteTX
        ( substituteTX
        , substituteTXs
        , substituteBoundTX
        , SubstituteTX(..))
where
import DDC.Core.Collect.FreeT
import DDC.Core.Exp
import DDC.Core.Transform.SpreadX
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified DDC.Type.Env           as Env


-- | Substitute a `Type` for the `Bound` corresponding to some `Bind` in a thing.
substituteTX :: (SubstituteTX c, Ord n) => Bind n -> Type n -> c n -> c n
substituteTX b t x
 = case takeSubstBoundOfBind b of
    Just u      -> substituteBoundTX u t x
    _           -> x


-- | Wrapper for `substituteT` to substitute multiple things.
substituteTXs :: (SubstituteTX c, Ord n) => [(Bind n, Type n)] -> c n -> c n
substituteTXs bts x
        = foldr (uncurry substituteTX) x bts


-- | Substitute a `Type` for `Bound` in some thing.
substituteBoundTX :: (SubstituteTX c, Ord n) => Bound n -> Type n -> c n -> c n
substituteBoundTX u t x
 = let -- Determine the free names in the type we're subsituting.
       -- We'll need to rename binders with the same names as these
       fnsT     = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeT Env.empty t

       stackT   = BindStack [] [] 0 0
 
  in   substituteWithTX u t fnsT stackT Env.empty x


-------------------------------------------------------------------------------
class SubstituteTX (c :: * -> *) where

 -- | Substitute a type into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithTX
        :: forall n. Ord n
        => Bound n       -- ^ Bound variable that we're subsituting into.
        -> Type n        -- ^ Type to substitute.
        -> Set  n        -- ^ Names of free type variables in the type to substitute.
        -> BindStack n   -- ^ Bind stack for rewriting type variables.
        -> Env n         -- ^ Current type environment.
        -> c n -> c n


instance SubstituteTX (Exp a) where
 substituteWithTX u t fnsT stackT envX xx
  = let down    = substituteWithTX u t fnsT stackT envX
    in  case xx of
         -- If we've substituted into the type annotation on a binder then we 
         -- also need to replace the annotation on the bound occurrences.
         XVar a u'
          -> case Env.lookup u' envX of
                Nothing -> xx
                Just t' -> XVar a $ replaceTypeOfBound t' u'

         XCon{}         -> xx
         XApp a x1 x2   -> XApp a (down x1) (down x2)

         XLAM a b xBody
          |  namedBoundMatchesBind u b
          -> spreadX (Env.fromList $ stackAll stackT) envX xx

          | otherwise
          -> let b2             = down b
                 (stackT', b3)  = pushBind fnsT stackT b2
                 xBody'         = substituteWithTX u t fnsT stackT' envX xBody
             in  XLAM a b3 xBody'

         XLam a b xBody
          -> let b'             = down b
                 envX'          = Env.extend b' envX
                 xBody'         = substituteWithTX u t fnsT stackT envX' xBody
             in  XLam a b' xBody'

         XLet a (LLet m b x1) x2
          -> let m'             = down m
                 b'             = down b
                 x1'            = down x1
                 envX'          = Env.extend b' envX
                 x2'            = substituteWithTX u t fnsT stackT envX' x2
             in  XLet a (LLet m' b' x1')  x2'

         XLet a (LRec bxs) x2
          -> let (bs, xs)       = unzip bxs
                 bs'            = map down bs
                 envX'          = Env.extends bs' envX
                 xs'            = map (substituteWithTX u t fnsT stackT envX') xs
                 x2'            = substituteWithTX u t fnsT stackT envX' x2
             in  XLet a (LRec (zip bs' xs')) x2'

         XLet a (LLetRegion b bs) x2
          |  namedBoundMatchesBind u b 
          -> spreadX (Env.fromList $ stackAll stackT) envX xx

          | otherwise
          -> let (stackT', b')  = pushBind fnsT stackT b
                 bs'            = map (substituteWithTX u t fnsT stackT' envX) bs
                 envX'          = Env.extends bs' envX
                 x2'            = substituteWithTX u t fnsT stackT' envX' x2
             in  XLet a (LLetRegion b' bs') x2'

         XLet a (LWithRegion uR) x2
           -> XLet a (LWithRegion uR) (down x2)

         XCase a x alts -> XCase  a (down x) (map down alts)
         XCast a c x    -> XCast  a (down c) (down x)
         XType t'       -> XType    (substituteWithT u t fnsT stackT t')
         XWitness w     -> XWitness (down w)


instance SubstituteTX LetMode where
 substituteWithTX u t fnsT stackT envX lm
  = let down    = substituteWithTX u t fnsT stackT envX 
    in case lm of
        LetStrict         -> lm
        LetLazy Nothing   -> lm
        LetLazy (Just w)  -> LetLazy (Just $ down w)


instance SubstituteTX (Alt a) where
 substituteWithTX u t fnsT stackT envX alt
  = let down    = substituteWithTX u t fnsT stackT envX
    in  case alt of
         AAlt PDefault x        
          -> AAlt PDefault (down x)

         AAlt (PData uCon bs) x
          -> let bs'     = map down bs
                 envX'   = Env.extends bs' envX
                 x'      = substituteWithTX u t fnsT stackT envX' x
             in  AAlt (PData uCon bs') x'


instance SubstituteTX Cast where
 substituteWithTX u t fnsT stackT envX tt
  = let down    = substituteWithTX u t fnsT stackT envX
    in  case tt of
         CastWeakenEffect eff   -> CastWeakenEffect  (substituteWithT u t fnsT stackT eff)
         CastWeakenClosure clo  -> CastWeakenClosure (substituteWithT u t fnsT stackT clo)
         CastPurify w           -> CastPurify (down w)
         CastForget w           -> CastForget (down w)


instance SubstituteTX Witness where
 substituteWithTX u t fvsT stackT envX ww
  = let down    = substituteWithTX u t fvsT stackT envX
    in case ww of
         WCon{}         -> ww

         WVar u'
          -> case Env.lookup u' envX of
                Nothing -> ww
                Just t' -> WVar $ replaceTypeOfBound t' u'
          
         WApp  w1 w2    -> WApp  (down w1) (down w2)
         WJoin w1 w2    -> WJoin (down w1) (down w2)
         WType t1       -> WType (substituteWithT u t fvsT stackT t1)


instance SubstituteTX Bind where
 substituteWithTX u t fnsT stackT _envX bb
  = let t'      = substituteWithT u t fnsT stackT $ typeOfBind bb
    in  replaceTypeOfBind t' bb

