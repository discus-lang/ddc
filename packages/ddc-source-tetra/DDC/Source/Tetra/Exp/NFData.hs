{-# LANGUAGE UndecidableInstances #-}
module DDC.Source.Tetra.Exp.NFData where
import DDC.Source.Tetra.Exp.Generic
import Control.DeepSeq

---------------------------------------------------------------------------------------------------
type NFDataLanguage l
        = ( NFData l
          , NFData (GAnnot l), NFData (GName l)
          , NFData (GBind l),  NFData (GBound l), NFData (GPrim l))

instance NFDataLanguage l => NFData (GExp l) where
 rnf xx
  = case xx of
        XAnnot    a x           -> rnf a `seq` rnf x
        XVar      u             -> rnf u
        XPrim     p             -> rnf p
        XCon      dc            -> rnf dc
        XLAM      b x           -> rnf b   `seq` rnf x
        XLam      b x           -> rnf b   `seq` rnf x
        XApp      x1 x2         -> rnf x1  `seq` rnf x2
        XLet      lts x         -> rnf lts `seq` rnf x
        XCase     x alts        -> rnf x   `seq` rnf alts
        XCast     c x           -> rnf c   `seq` rnf x
        XType     t             -> rnf t
        XWitness  w             -> rnf w
        XDefix    a xs          -> rnf a   `seq` rnf xs
        XInfixOp  a s           -> rnf a   `seq` rnf s
        XInfixVar a s           -> rnf a   `seq` rnf s


instance NFDataLanguage l => NFData (GClause l) where
 rnf cc
  = case cc of
        SSig a b t              -> rnf a `seq` rnf b `seq` rnf t
        SLet a b ps gxs         -> rnf a `seq` rnf b `seq` rnf ps `seq` rnf gxs


instance NFDataLanguage l => NFData (GLets l) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LPrivate bs1 mR bs2     -> rnf bs1  `seq` rnf mR `seq` rnf bs2
        LGroup cs               -> rnf cs


instance NFDataLanguage l => NFData (GAlt l) where
 rnf aa
  = case aa of
        AAlt w gxs              -> rnf w `seq` rnf gxs


instance NFDataLanguage l => NFData (GPat l) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PData dc bs             -> rnf dc `seq` rnf bs


instance NFDataLanguage l => NFData (GGuardedExp l) where
 rnf gx
  = case gx of
        GGuard g gx'            -> rnf g `seq` rnf gx'
        GExp x                  -> rnf x


instance NFDataLanguage l => NFData (GGuard l) where
 rnf gg
  = case gg of
        GPred x                 -> rnf x
        GPat  p x               -> rnf p `seq` rnf x
        GDefault                -> ()


instance NFDataLanguage l => NFData (GCast l) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastPurify w            -> rnf w
        CastBox                 -> ()
        CastRun                 -> ()


instance NFDataLanguage l => NFData (GWitness l) where
 rnf ww
  = case ww of
        WAnnot a w              -> rnf a `seq` rnf w
        WVar   u                -> rnf u
        WCon   c                -> rnf c
        WApp   w1 w2            -> rnf w1 `seq` rnf w2
        WType  t                -> rnf t


instance NFDataLanguage l => NFData (GWiCon l) where
 rnf wc
  = case wc of
        WiConBound u t          -> rnf u `seq` rnf t

