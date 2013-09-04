
module DDC.Core.Transform.Reannotate
        (Reannotate (..))
where
import DDC.Core.Module
import DDC.Core.Exp


-- | Apply the given function to every annotation in a core thing.
class Reannotate c where
 reannotate :: (a -> b) -> c a n -> c b n


instance Reannotate Module where
 reannotate f
     (ModuleCore name 
                 exportKinds exportTypes 
                 importKinds importTypes
                 dataDefsLocal
                 body)
  =   ModuleCore name
                 exportKinds exportTypes
                 importKinds importTypes
                 dataDefsLocal
                 (reannotate f body)


instance Reannotate Exp where
 reannotate f xx
  = let down x   = reannotate f x
    in case xx of
        XVar     a u            -> XVar     (f a) u
        XCon     a u            -> XCon     (f a) u
        XLAM     a b x          -> XLAM     (f a) b (down x)
        XLam     a b x          -> XLam     (f a) b (down x)
        XApp     a x1 x2        -> XApp     (f a)   (down x1)  (down x2)
        XLet     a lts x        -> XLet     (f a)   (down lts) (down x)
        XCase    a x alts       -> XCase    (f a)   (down x)   (map down alts)
        XCast    a c x          -> XCast    (f a)   (down c)   (down x)
        XType    a t            -> XType    (f a) t
        XWitness a w            -> XWitness (f a)   (down w)


instance Reannotate Lets where
 reannotate f xx
  = let down x  = reannotate f x
    in case xx of
        LLet b x                -> LLet b (down x)
        LRec bxs                -> LRec [(b, down x) | (b, x) <- bxs]
        LLetRegions b bs        -> LLetRegions b bs
        LWithRegion b           -> LWithRegion b


instance Reannotate Alt where
 reannotate f aa
  = case aa of
        AAlt w x                -> AAlt w (reannotate f x)


instance Reannotate Cast where
 reannotate f cc
  = let down x  = reannotate f x
    in case cc of
        CastWeakenEffect  eff   -> CastWeakenEffect eff
        CastWeakenClosure xs    -> CastWeakenClosure (map down xs)
        CastPurify w            -> CastPurify (down w)
        CastForget w            -> CastForget (down w)
        CastSuspend             -> CastSuspend
        CastRun                 -> CastRun


instance Reannotate Witness where
 reannotate f ww
  = let down x = reannotate f x
    in case ww of
        WVar  a u               -> WVar  (f a) u
        WCon  a c               -> WCon  (f a) c
        WApp  a w1 w2           -> WApp  (f a) (down w1) (down w2)
        WJoin a w1 w2           -> WJoin (f a) (down w1) (down w2)
        WType a t               -> WType (f a) t

