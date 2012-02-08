
-- | Type substitution.
module DDC.Core.Transform.SubstituteTX
where
import DDC.Core.Collect.FreeT
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import Data.List
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


-- TODO: Need to push both type and value names so we can switch to 
--       spread mode when we hit a binder with the same name.
instance SubstituteTX (Exp a) where
 substituteWithTX u t fnsT stackT envX xx
  = let down    = substituteWithTX u t fnsT stackT envX
    in  case xx of
         -- If we've substituted into the type annotation on a binder
         -- further up, then we also need to replace the annotation
         -- on the bound occurrence with this new type.
         XVar a u'
          -> case u' of
                UIx i _ 
                 -> case lookup i (zip [0..] (stackAll stackT)) of
                     Just b  
                       | not $ isBot $ typeOfBind b  
                       -> XVar a (UIx i $ typeOfBind b)
                     _ -> xx

                UName n _ 
                 -> case find (boundMatchesBind u') (stackAll stackT) of
                     Just b  
                       | not $ isBot $ typeOfBind b
                       -> XVar a (UName n $ typeOfBind b)
                     _ -> xx

                UPrim{} -> xx

         XCon{} -> xx
         
         XApp a x1 x2           
          -> XApp a (down x1) (down x2)

         XLAM a b xBody                                 -- TODO switch to spread mode if bound matches bind
          -> let b2             = down b
                 (stack', b3)   = pushBind fnsT stackT b2
                 xBody'         = substituteWithTX u t fnsT stack' envX xBody
             in  XLAM a b3 xBody'

         XLam a b xBody
          -> let b2             = down b
                 (stack', b3)   = pushBind fnsT stackT b2
                 xBody'         = substituteWithTX u t fnsT stack' envX xBody
             in  XLam a b3 xBody'

         XLet a (LLet m b x1) x2
          -> let x1'            = down x1
                 (stack', b')   = pushBind fnsT stackT (down b)
                 x2'            = substituteWithTX u t fnsT stack' envX x2
             in  XLet a (LLet m b' x1')  x2'

         XLet a (LRec bxs) x2
          -> let (bs, xs)       = unzip bxs
                 (stack', bs')  = pushBinds fnsT stackT (map down bs)
                 xs'            = map (substituteWithTX u t fnsT stack' envX) xs
                 x2'            = substituteWithTX u t fnsT stack' envX x2
             in  XLet a (LRec (zip bs' xs')) x2'

         XLet a (LLetRegion b bs) x2
          -> let (stack1, b')   = pushBind  fnsT stackT (down b)
                 (stack2, bs')  = pushBinds fnsT stack1 (map down bs)
                 x2'            = substituteWithTX u t fnsT stack2 envX x2
             in  XLet a (LLetRegion b' bs') x2'

         XLet a (LWithRegion uR) x2
           -> XLet a (LWithRegion uR) (down x2)

         XCase a x alts
          -> XCase a (down x) (map down alts)

         XCast a c x
          -> XCast a (down c) (down x)
         
         XType t'         -> XType    (substituteWithT  u t fnsT stackT t')
         XWitness w       -> XWitness (substituteWithTX u t fnsT stackT envX w)


instance SubstituteTX Pat where
 substituteWithTX u t fnsT stackT envX pat
  = case pat of
        PDefault        -> PDefault
        PData uCon bs   -> PData uCon (map (substituteWithTX u t fnsT stackT envX) bs)


instance SubstituteTX (Alt a) where
 substituteWithTX u t fnsT stackT envX (AAlt pat x)
  = let down    = substituteWithTX u t fnsT stackT envX
    in  AAlt (down pat) (down x)


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
          -> let t'  = substituteWithT u t fvsT stackT (typeOfBound u')
             in  WVar $ replaceTypeOfBound t' u'
          
         WApp  w1 w2    -> WApp  (down w1) (down w2)
         WJoin w1 w2    -> WJoin (down w1) (down w2)
         WType t1       -> WType (substituteWithT u t fvsT stackT t1)


instance SubstituteTX Bind where
 substituteWithTX u t fnsT stackT _envX bb
  = let k'      = substituteWithT u t fnsT stackT $ typeOfBind bb
    in  replaceTypeOfBind k' bb

