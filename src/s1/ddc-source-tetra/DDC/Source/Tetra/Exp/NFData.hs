{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UndecidableInstances #-}
module DDC.Source.Tetra.Exp.NFData where
import DDC.Source.Tetra.Exp.Generic
import qualified DDC.Type.Exp.Generic.NFData    as T
import Control.DeepSeq

---------------------------------------------------------------------------------------------------
type NFDataLanguage l
        = ( NFData l, T.NFDataLanguage l
          , NFData (GXAnnot   l)
          , NFData (GXBindVar l),  NFData (GXBoundVar l)
          , NFData (GXBindCon l),  NFData (GXBoundCon l)
          , NFData (GXFrag    l))

instance NFDataLanguage l => NFData (GExp l) where
 rnf xx
  = case xx of
        XAnnot    a x           -> rnf a `seq` rnf x
        XPrim     _             -> ()
        XFrag     p             -> rnf p
        XVar      u             -> rnf u
        XCon      dc            -> rnf dc
        XAbs      p x           -> rnf p   `seq` rnf x
        XApp      x1 a2         -> rnf x1  `seq` rnf a2
        XLet      lts x         -> rnf lts `seq` rnf x
        XCase     x alts        -> rnf x   `seq` rnf alts
        XCast     c x           -> rnf c   `seq` rnf x
        XDefix    a xs          -> rnf a   `seq` rnf xs
        XInfixOp  a s           -> rnf a   `seq` rnf s
        XInfixVar a s           -> rnf a   `seq` rnf s
        XMatch    a as x        -> rnf a   `seq` rnf as `seq` rnf x
        XWhere    a cl x        -> rnf a   `seq` rnf cl `seq` rnf x
        XAbsPat   a _ps w mt x  -> rnf a   `seq` rnf w  `seq` rnf mt `seq` rnf x
        XLamCase  a as          -> rnf a   `seq` rnf as


instance NFDataLanguage l => NFData (GXBindVarMT l) where
 rnf (XBindVarMT b mt)
  = rnf b `seq` rnf mt


instance NFDataLanguage l => NFData (GClause l) where
 rnf cc
  = case cc of
        SSig a b t              -> rnf a `seq` rnf b `seq` rnf t
        SLet a b ps gxs         -> rnf a `seq` rnf b `seq` rnf ps `seq` rnf gxs


instance NFDataLanguage l => NFData (GParam l) where
 rnf pp
  = case pp of
        MType     b mt          -> rnf b `seq` rnf mt
        MTerm     p mt          -> rnf p `seq` rnf mt
        MImplicit p mt          -> rnf p `seq` rnf mt


instance NFDataLanguage l => NFData (GArg l) where
 rnf aa
  = case aa of
        RType t                 -> rnf t
        RTerm x                 -> rnf x
        RImplicit x             -> rnf x
        RWitness  w             -> rnf w


instance NFDataLanguage l => NFData (GLets l) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LPrivate bs1 mR bs2     -> rnf bs1  `seq` rnf mR `seq` rnf bs2
        LGroup _bRec cs         -> rnf cs


instance NFDataLanguage l => NFData (GAltCase l) where
 rnf aa
  = case aa of
        AAltCase w gxs          -> rnf w `seq` rnf gxs


instance NFDataLanguage l => NFData (GAltMatch l) where
 rnf aa
  = case aa of
        AAltMatch gs            -> rnf gs


instance NFDataLanguage l => NFData (GPat l) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PAt   b p               -> rnf b  `seq` rnf p
        PVar  b                 -> rnf b
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

