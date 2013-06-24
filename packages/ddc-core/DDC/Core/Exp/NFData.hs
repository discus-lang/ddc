
module DDC.Core.Exp.NFData where
import DDC.Core.Exp.Base
import DDC.Core.Exp.WiCon
import Control.DeepSeq


instance (NFData a, NFData n) => NFData (Exp a n) where
 rnf xx
  = case xx of
        XVar  a u       -> rnf a `seq` rnf u
        XCon  a dc      -> rnf a `seq` rnf dc
        XLAM  a b x     -> rnf a `seq` rnf b   `seq` rnf x
        XLam  a b x     -> rnf a `seq` rnf b   `seq` rnf x
        XApp  a x1 x2   -> rnf a `seq` rnf x1  `seq` rnf x2
        XLet  a lts x   -> rnf a `seq` rnf lts `seq` rnf x
        XCase a x alts  -> rnf a `seq` rnf x   `seq` rnf alts
        XCast a c x     -> rnf a `seq` rnf c   `seq` rnf x
        XType t         -> rnf t
        XWitness w      -> rnf w


instance (NFData a, NFData n) => NFData (Cast a n) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastWeakenClosure xs    -> rnf xs
        CastPurify w            -> rnf w
        CastForget w            -> rnf w


instance (NFData a, NFData n) => NFData (Lets a n) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LLetRegions bs1 bs2     -> rnf bs1  `seq` rnf bs2
        LWithRegion u           -> rnf u


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
        WVar  a u                 -> rnf a `seq` rnf u
        WCon  a c                 -> rnf a `seq` rnf c
        WApp  a w1 w2             -> rnf a `seq` rnf w1 `seq` rnf w2
        WJoin a w1 w2             -> rnf a `seq` rnf w1 `seq` rnf w2
        WType a tt                -> rnf a `seq` rnf tt


instance NFData n => NFData (WiCon n) where
 rnf wi
  = case wi of
        WiConBuiltin wb         -> rnf wb
        WiConBound   u t        -> rnf u `seq` rnf t

instance NFData WbCon






