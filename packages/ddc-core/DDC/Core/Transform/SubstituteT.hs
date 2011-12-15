
-- | Type substitution.
module DDC.Core.Transform.SubstituteT
        ( SubstituteT(..)
        , substituteT
        , substituteTs)
where
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Transform.SubstituteT


instance SubstituteT (Exp a) where
 substituteWithT u t fvs stack xx
  = let down    = substituteWithT u t fvs stack
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

         XLet{}  -> error "substituteWithT: XLet  not done yet"
         
         XLam{}  -> error "substituteWithT: XLam  not done yet"
         XCase{} -> error "substituteWithT: XCase not done yet"
         XCast{} -> error "substituteWithT: XCast not done yet"

         
         XType t'         -> XType    (down t')
         XWitness w       -> XWitness (down w)


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
