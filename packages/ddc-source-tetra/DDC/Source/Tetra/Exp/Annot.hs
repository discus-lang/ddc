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
import DDC.Source.Tetra.Prim
import qualified DDC.Type.Exp           as T


-- | Type index for annotated expression type.
data Annot a

instance HasAnonBind (Annot a) where
 isAnon _ (T.BAnon _)   = True
 isAnon _ _             = False

type instance GName  (Annot a) = Name
type instance GAnnot (Annot a) = a
type instance GBind  (Annot a) = T.Bind  Name
type instance GBound (Annot a) = T.Bound Name
type instance GPrim  (Annot a) = PrimVal

type Bound              = T.Bound Name
type Bind               = T.Bind  Name
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
