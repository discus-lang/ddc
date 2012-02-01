
module DDC.Core.Transform.Anonymize
        (Anonymize(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.Anonymize
import Control.Monad


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
         -> let (stack', b')    = pushAnonymizeBind stack b
            in  XLam a b' (anonymize stack' x)

        XLet a lts x
         -> let (stack', lts')  = pushAnonymizeLets stack lts
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


instance Anonymize LetMode where
 anonymize stack lm
  = case lm of
        LetStrict       -> lm
        LetLazy mw      -> LetLazy $ liftM (anonymize stack) mw


instance Anonymize (Alt a) where
 anonymize = error "anonymize alts"

instance Anonymize Witness where
 anonymize = error "anonymize witness"


pushAnonymizeLets :: Ord n => [Bind n] -> Lets a n -> ([Bind n], Lets a n)
pushAnonymizeLets stack lts
 = case lts of
        LLet mode b x
         -> let mode'           = anonymize stack mode
                (stack', b')    = pushAnonymizeBind stack b
                x'              = anonymize stack x
            in  (stack', LLet mode' b' x')

        LLetRegion b bs
         -> let (stack1,  b')   = pushAnonymizeBind stack b
                (stack2, bs')   = pushAnonymizeBinds stack1 bs
            in  (stack2, LLetRegion b' bs')

        _ -> error "anonymize lets"
