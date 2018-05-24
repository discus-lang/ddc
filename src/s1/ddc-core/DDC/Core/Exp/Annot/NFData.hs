{-# OPTIONS_HADDOCK hide #-}
-- | `NFData` instances for Core AST types.
module DDC.Core.Exp.Annot.NFData where
import DDC.Core.Exp.Annot.Exp
import Control.DeepSeq


instance (NFData a, NFData n) => NFData (Exp a n) where
 rnf xx
  = case xx of
        XVar   a u           -> rnf a `seq` rnf u
        XAbs   a b x         -> rnf a `seq` rnf b   `seq` rnf x
        XApp   a x1 x2       -> rnf a `seq` rnf x1  `seq` rnf x2
        XLet   a lts x       -> rnf a `seq` rnf lts `seq` rnf x
        XAtom  a t           -> rnf a `seq` rnf t
        XCase  a x alts      -> rnf a `seq` rnf x   `seq` rnf alts
        XCast  a c x         -> rnf a `seq` rnf c   `seq` rnf x
        XAsync a v e1 e2     -> rnf a `seq` rnf v   `seq` rnf e1   `seq` rnf e2

instance (NFData a, NFData n) => NFData (Arg a n) where
 rnf rr
  = case rr of
        RType t                 -> rnf t
        RTerm x                 -> rnf x
        RWitness  x             -> rnf x
        RImplicit x             -> rnf x


instance NFData n => NFData (Atom n) where
 rnf aa
  = case aa of
        MACon dc                -> rnf dc
        MALabel l               -> rnf l
        MAPrim _                -> ()


instance (NFData n)  => NFData (Param n) where
 rnf mm
  = case mm of
        MTerm b                 -> rnf b
        MType b                 -> rnf b
        MImplicit b             -> rnf b


instance (NFData a, NFData n) => NFData (Cast a n) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastPurify w            -> rnf w
        CastBox                 -> ()
        CastRun                 -> ()


instance (NFData a, NFData n) => NFData (Lets a n) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LPrivate bs1 u2 bs3     -> rnf bs1 `seq` rnf u2 `seq` rnf bs3


instance (NFData a, NFData n) => NFData (Alt a n) where
 rnf aa
  = case aa of
        AAlt w x                -> rnf w `seq` rnf x


instance NFData n => NFData (Pat n) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PData dc bs             -> rnf dc `seq` rnf bs


instance (NFData a, NFData n) => NFData (Witness a n) where
 rnf ww
  = case ww of
        WVar  a u               -> rnf a `seq` rnf u
        WCon  a c               -> rnf a `seq` rnf c
        WApp  a w1 w2           -> rnf a `seq` rnf w1 `seq` rnf w2
        WType a tt              -> rnf a `seq` rnf tt
