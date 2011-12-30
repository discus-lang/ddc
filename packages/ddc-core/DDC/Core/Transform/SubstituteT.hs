
-- | Type substitution.
module DDC.Core.Transform.SubstituteT
        (module DDC.Type.Transform.SubstituteT)
where
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT


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



instance SubstituteT (Exp a) where
 substituteWithT u t fns stack xx
  = let down    = substituteWithT u t fns stack
    in  case xx of
         -- Types are never substitute for expression variables, but we do need
         -- to substitute into the annotation.
         XVar a u'
          -> let t' = down (typeOfBound u')
             in  XVar a $ replaceTypeOfBound t' u'
             
         XCon a u'
          -> let t' = down (typeOfBound u')
             in  XCon a $ replaceTypeOfBound t' u'
         
         XApp a x1 x2           
          -> XApp a (down x1) (down x2)

         XLet a (LLet b x1) x2
          -> XLet a (LLet (down b) (down x1)) (down x2)

         XLet{}  -> error "substituteWithT: XLet not done yet"

         XLam a b xBody
          -> let b2             = down b
                 (stack', b3)   = pushBind fns stack b2
                 xBody'         = substituteWithT u t fns stack' xBody
             in  XLam a b3 xBody'


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

