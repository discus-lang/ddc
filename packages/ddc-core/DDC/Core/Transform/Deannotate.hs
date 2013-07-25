
module DDC.Core.Transform.Deannotate
        (Deannotate(..))
where
import qualified DDC.Core.Exp.Annot     as A
import qualified DDC.Core.Exp.Simple    as S


-- | Convert the `Annot` version of the AST to the `Simple` version,
--   using the provided function to decide when to keep the annotation.
class Deannotate 
        (c1 :: * -> * -> *)
        (c2 :: * -> * -> *) | c1 -> c2
 where  
 deannotate :: (a -> Maybe a) -> c1 a n -> c2 a n


instance Deannotate A.Exp S.Exp where
 deannotate f xx
  = let down      = deannotate f 
        wrap a x  = case f a of
                        Nothing -> x
                        Just a' -> S.XAnnot a' x
    in case xx of
        A.XVar  a u             -> wrap a (S.XVar u)
        A.XCon  a dc            -> wrap a (S.XCon dc)
        A.XLAM  a b x           -> wrap a (S.XLAM b (down x))
        A.XLam  a b x           -> wrap a (S.XLam b (down x))
        A.XApp  a x1 x2         -> wrap a (S.XApp   (down x1)  (down x2))
        A.XLet  a lts x2        -> wrap a (S.XLet   (down lts) (down x2))
        A.XCase a x alts        -> wrap a (S.XCase  (down x)   (map down alts))
        A.XCast a cc x          -> wrap a (S.XCast  (down cc)  (down x))
        A.XType t               -> S.XType t
        A.XWitness w            -> S.XWitness (down w)


instance Deannotate A.Lets S.Lets where
 deannotate f lts
  = let down    = deannotate f
    in case lts of
        A.LLet b x              -> S.LLet b (down x)
        A.LRec bxs              -> S.LRec [(b, down x) | (b, x) <- bxs]
        A.LLetRegions bks bts   -> S.LLetRegions bks bts
        A.LWithRegion u         -> S.LWithRegion u


instance Deannotate A.Alt S.Alt where
 deannotate f aa
  = case aa of
        A.AAlt w x              -> S.AAlt w (deannotate f x)


instance Deannotate A.Witness S.Witness where
 deannotate f ww
  = let down     = deannotate f
        wrap a x = case f a of
                        Nothing -> x
                        Just a' -> S.WAnnot a' x
    in case ww of
        A.WVar  a u             -> wrap a (S.WVar u)
        A.WCon  a wc            -> wrap a (S.WCon wc)
        A.WApp  a w1 w2         -> wrap a (S.WApp  (down w1) (down w2))
        A.WJoin a w1 w2         -> wrap a (S.WJoin (down w1) (down w2))
        A.WType a t             -> wrap a (S.WType t)


instance Deannotate A.Cast S.Cast where
 deannotate f cc
  = let down    = deannotate f
    in case cc of
        A.CastWeakenEffect e    -> S.CastWeakenEffect e
        A.CastWeakenClosure xs  -> S.CastWeakenClosure (map down xs)
        A.CastPurify w          -> S.CastPurify (down w)
        A.CastForget w          -> S.CastForget (down w)
        A.CastSuspend           -> S.CastSuspend
        A.CastRun               -> S.CastRun


