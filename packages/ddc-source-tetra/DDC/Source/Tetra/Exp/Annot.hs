{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Exp.Annot
        ( module DDC.Source.Tetra.Exp.Generic
        , Annot
        , HasAnonBind   (..)


        -- * Expressions
        , Name       
        , Bound
        , Bind
        , Exp        
        , Lets       
        , Alt        
        , Pat        
        , Clause     
        , GuardedExp 
        , Guard      
        , Cast
        , DaCon         (..)

        -- * Witnesses
        , Witness
        , WiCon)
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Bind
import DDC.Source.Tetra.Prim


-- | Type index for annotated expression type.
data Annot a

instance HasAnonBind (Annot a) where
 isAnon _ BAnon         = True
 isAnon _ _             = False

type instance GXAnnot    (Annot a) = a
type instance GXBindVar  (Annot a) = Bind
type instance GXBoundVar (Annot a) = Bound
type instance GXPrim     (Annot a) = PrimVal

type Exp        a       = GExp          (Annot a)
type Lets       a       = GLets         (Annot a)
type Alt        a       = GAlt          (Annot a)
type Pat        a       = GPat          (Annot a)
type Clause     a       = GClause       (Annot a)
type GuardedExp a       = GGuardedExp   (Annot a)
type Guard      a       = GGuard        (Annot a)
type Cast       a       = GCast         (Annot a)
type Witness    a       = GWitness      (Annot a)
type WiCon      a       = GWiCon        (Annot a)
