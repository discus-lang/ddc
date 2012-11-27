
module DDC.Type.Exp.NFData where
import DDC.Type.Exp.Base
import Control.DeepSeq


instance NFData n => NFData (Binder n) where
 rnf bb
  = case bb of
        RNone   -> ()
        RAnon   -> ()
        RName n -> rnf n


instance NFData n => NFData (Bind n) where
 rnf bb
  = case bb of
        BNone t         -> rnf t
        BAnon t         -> rnf t
        BName n t       -> rnf n `seq` rnf t


instance NFData n => NFData (Bound n) where
 rnf uu
  = case uu of
        UIx   i         -> rnf i
        UName n         -> rnf n
        UPrim u t       -> rnf u `seq` rnf t


instance NFData n => NFData (Type n) where
 rnf tt
  = case tt of
        TVar u          -> rnf u
        TCon tc         -> rnf tc
        TForall b t     -> rnf b  `seq` rnf t
        TApp    t1 t2   -> rnf t1 `seq` rnf t2
        TSum    ts      -> rnf ts


instance NFData n => NFData (TypeSum n) where
 rnf !ts
  = case ts of
        TypeSumBot{}
         -> rnf (typeSumKind ts)

        TypeSumSet{}    
         ->    rnf (typeSumKind       ts)
         `seq` rnf (typeSumElems      ts)
         `seq` rnf (typeSumBoundNamed ts)
         `seq` rnf (typeSumBoundAnon  ts)
         `seq` rnf (typeSumSpill      ts)


instance NFData TyConHash where
 rnf (TyConHash i)
  = rnf i


instance NFData n => NFData (TypeSumVarCon n) where
 rnf ts
  = case ts of
        TypeSumVar u    -> rnf u
        TypeSumCon u t  -> rnf u `seq` rnf t


instance NFData n => NFData (TyCon n) where
 rnf tc
  = case tc of
        TyConSort    con        -> rnf con
        TyConKind    con        -> rnf con
        TyConWitness con        -> rnf con
        TyConSpec    con        -> rnf con
        TyConBound   con k      -> rnf con `seq` rnf k


instance NFData SoCon
instance NFData KiCon
instance NFData TwCon
instance NFData TcCon

