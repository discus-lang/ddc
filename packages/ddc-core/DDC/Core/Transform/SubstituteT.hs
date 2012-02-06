
-- | Type substitution.
module DDC.Core.Transform.SubstituteT
        (module DDC.Type.Transform.SubstituteT)
where
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Transform.SubstituteT
import Data.List


instance SubstituteT Witness where
 substituteWithT u t fvs stack ww
  = let down    = substituteWithT u t fvs stack
    in case ww of
         WCon{}         -> ww

         WVar u'
          -> let t'  = down (typeOfBound u')
             in  WVar $ replaceTypeOfBound t' u'
          
         WApp  w1 w2    -> WApp  (down w1) (down w2)
         WJoin w1 w2    -> WJoin (down w1) (down w2)
         WType t1       -> WType (down t1)


-- TODO: Need to push both type and value names so we can switch to 
--       spread mode when we hit a binder with the same name.
instance SubstituteT (Exp a) where
 substituteWithT u t fns stack xx
  = let down    = substituteWithT u t fns stack
    in  case xx of
         -- If we've substituted into the type annotation on a binder
         -- further up, then we also need to replace the annotation
         -- on the bound occurrence with this new type.
         XVar a u'
          -> case u' of
                UIx i _ 
                 -> case lookup i (zip [0..] (stackAll stack)) of
                     Just b  
                       | not $ isBot $ typeOfBind b  
                       -> XVar a (UIx i $ typeOfBind b)
                     _ -> xx

                UName n _ 
                 -> case find (boundMatchesBind u') (stackAll stack) of
                     Just b  
                       | not $ isBot $ typeOfBind b
                       -> XVar a (UName n $ typeOfBind b)
                     _ -> xx

                UPrim{} -> xx

         XCon{} -> xx
         
         XApp a x1 x2           
          -> XApp a (down x1) (down x2)

         XLAM a b xBody
          -- TODO: switch to spread mode if u matches b, 
          -- still need to update types.
          -> let b2             = down b
                 (stack', b3)   = pushBind fns stack b2
                 xBody'         = substituteWithT u t fns stack' xBody
             in  XLAM a b3 xBody'

         XLam a b xBody
          -> let b2             = down b
                 (stack', b3)   = pushBind fns stack b2
                 xBody'         = substituteWithT u t fns stack' xBody
             in  XLam a b3 xBody'

         XLet a (LLet m b x1) x2
          -> let x1'            = down x1
                 (stack', b')   = pushBind fns stack (down b)
                 x2'            = substituteWithT u t fns stack' x2
             in  XLet a (LLet m b' x1')  x2'

         XLet a (LRec bxs) x2
          -> let (bs, xs)       = unzip bxs
                 (stack', bs')  = pushBinds fns stack (map down bs)
                 xs'            = map (substituteWithT u t fns stack') xs
                 x2'            = substituteWithT u t fns stack' x2
             in  XLet a (LRec (zip bs' xs')) x2'

         XLet a (LLetRegion b bs) x2
          -> let (stack1, b')   = pushBind  fns stack  (down b)
                 (stack2, bs')  = pushBinds fns stack1 (map down bs)
                 x2'            = substituteWithT u t fns stack2 x2
             in  XLet a (LLetRegion b' bs') x2'

         XLet a (LWithRegion uR) x2
           -> XLet a (LWithRegion uR) (down x2)

         XCase a x alts
          -> XCase a (down x) (map down alts)

         XCast a c x
          -> XCast a (down c) (down x)
         
         XType t'         -> XType    (down t')
         XWitness w       -> XWitness (down w)


instance SubstituteT Pat where
 substituteWithT u t fns stack pat
  = case pat of
        PDefault        -> PDefault
        PData uCon bs   -> PData uCon (map (substituteWithT u t fns stack) bs)


instance SubstituteT (Alt a) where
 substituteWithT u t fns stack (AAlt pat x)
  = let down    = substituteWithT u t fns stack
    in  AAlt (down pat) (down x)


instance SubstituteT Cast where
 substituteWithT u t fns stack tt
  = let down    = substituteWithT u t fns stack
    in  case tt of
         CastWeakenEffect eff   -> CastWeakenEffect  (down eff)
         CastWeakenClosure clo  -> CastWeakenClosure (down clo)
         CastPurify w           -> CastPurify (down w)
         CastForget w           -> CastForget (down w)

