{-# OPTIONS_HADDOCK hide #-}

module DDC.Source.Discus.Exp.Term.NFData where
import DDC.Source.Discus.Exp.Term.Base
import DDC.Source.Discus.Exp.Type.NFData  ()
import Control.DeepSeq

---------------------------------------------------------------------------------------------------
instance NFData l => NFData (GExp l) where
 rnf xx
  = case xx of
        XAnnot    a x           -> rnf a `seq` rnf x
        XPrim     _             -> ()
        XVar      _             -> ()
        XCon      _             -> ()
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


instance NFData l => NFData (GXBindVarMT l) where
 rnf (XBindVarMT _ mt)
  = rnf mt


instance NFData l => NFData (GClause l) where
 rnf cc
  = case cc of
        SSig a _ t              -> rnf a `seq` rnf t
        SLet a _ ps gxs         -> rnf a `seq` rnf ps `seq` rnf gxs


instance NFData l => NFData (GParam l) where
 rnf pp
  = case pp of
        MType     _ mt          -> rnf mt
        MTerm     p mt          -> rnf p `seq` rnf mt
        MImplicit p mt          -> rnf p `seq` rnf mt


instance NFData l => NFData (GArg l) where
 rnf aa
  = case aa of
        RType t                 -> rnf t
        RTerm x                 -> rnf x
        RImplicit x             -> rnf x
        RWitness  w             -> rnf w


instance NFData l => NFData (GLets l) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LPrivate{}              -> ()
        LExtend _ t _           -> rnf t
        LGroup _bRec cs         -> rnf cs


instance NFData l => NFData (GCaps l) where
 rnf caps
  = case caps of
        CapsList _              -> ()
        CapsMutable             -> ()
        CapsConstant            -> ()


instance NFData l => NFData (GAltCase l) where
 rnf aa
  = case aa of
        AAltCase w gxs          -> rnf w `seq` rnf gxs


instance NFData l => NFData (GAltMatch l) where
 rnf aa
  = case aa of
        AAltMatch gs            -> rnf gs


instance NFData l => NFData (GPat l) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PAt   _ p               -> rnf p
        PVar  _                 -> ()
        PData _ bs              -> rnf bs


instance NFData l => NFData (GGuardedExp l) where
 rnf gx
  = case gx of
        GGuard g gx'            -> rnf g `seq` rnf gx'
        GExp x                  -> rnf x


instance NFData l => NFData (GGuard l) where
 rnf gg
  = case gg of
        GPred x                 -> rnf x
        GPat  p x               -> rnf p `seq` rnf x
        GDefault                -> ()


instance NFData l => NFData (GCast l) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastBox                 -> ()
        CastRun                 -> ()


instance NFData l => NFData (GWitness l) where
 rnf ww
  = case ww of
        WAnnot a w              -> rnf a `seq` rnf w
        WVar   _u               -> ()
        WCon   c                -> rnf c
        WApp   w1 w2            -> rnf w1 `seq` rnf w2
        WType  t                -> rnf t


instance NFData l => NFData (GWiCon l) where
 rnf wc
  = case wc of
        WiConBound _ t          -> rnf t


instance NFData PrimLit where
 rnf lit
  = case lit of
        PrimLitBool     b       -> rnf b
        PrimLitNat      n       -> rnf n
        PrimLitInt      i       -> rnf i
        PrimLitSize     s       -> rnf s
        PrimLitWord     i bits  -> rnf i `seq` rnf bits
        PrimLitFloat    d bits  -> rnf d `seq` rnf bits
        PrimLitChar     c       -> rnf c
        PrimLitTextLit  bs      -> rnf bs


instance NFData PrimVal where
 rnf val
  = case val of
        PrimValError    p       -> rnf p
        PrimValLit      lit     -> rnf lit
        PrimValArith    p       -> rnf p
        PrimValCast     p       -> rnf p
        PrimValVector   p       -> rnf p
        PrimValFun      p       -> rnf p
        PrimValElaborate        -> ()
        PrimValProject _        -> ()
        PrimValShuffle          -> ()
        PrimValCombine          -> ()

