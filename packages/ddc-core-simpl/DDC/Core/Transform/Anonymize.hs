
module DDC.Core.Transform.Anonymize
        (Anonymize(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.Anonymize


instance Anonymize (Exp a) where
 anonymize stack xx
  = case xx of
        XVar a u
         -> XVar a (anonymize stack u)

        XCon a u
         -> XVar a (anonymize stack u)

        XApp a x1 x2
         -> XApp a (anonymize stack x1) (anonymize stack x2)

        XLam a b x
         -> let (b', stack')    = pushAnonymizeBind b stack
            in  XLam a b' (anonymize stack' x)

        XLet a lts x
         -> let (lts', stack')  = pushAnonymizeLets lts stack
            in  XLet a lts' (anonymize stack' x)

        XCase a x alts
         -> XCase a (anonymize stack x) (map (anonymize stack) alts)
        
        XCast a c x
         -> XCast a (anonymize stack c) (anonymize stack x)

        XType t         -> XType    (anonymize stack t)
        XWitness t      -> XWitness (anonymize stack t)


instance Anonymize Cast where
 anonymize stack cc
  = case cc of
        CastWeakenEffect eff
         -> CastWeakenEffect  (anonymize stack eff)
        
        CastWeakenClosure clo
         -> CastWeakenClosure (anonymize stack clo)

        CastPurify w
         -> CastPurify (anonymize stack w)

        CastForget w
         -> CastForget (anonymize stack w)


instance Anonymize (Lets a) where
 anonymize = error "anonymize lets"

instance Anonymize (Alt a) where
 anonymize = error "anonymize alts"

instance Anonymize Witness where
 anonymize = error "anonymize witness"


pushAnonymizeLets :: Ord n => Lets a n -> [Bind n] -> (Lets a n, [Bind n])
pushAnonymizeLets 
        = error "anonymize lets"
