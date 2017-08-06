{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Tetra.Convert.Prim
        ( toCoreTyConTetra
        , toCorePrimVal
        , toCorePrimLit)
where
import qualified DDC.Core.Tetra.Prim    as C
import qualified DDC.Source.Tetra.Prim  as S


-- | Convert a Tetra specific type constructor to core.
toCoreTyConTetra :: S.PrimTyConTetra -> C.TyConTetra
toCoreTyConTetra tc
 = case tc of
        S.PrimTyConTetraTuple n -> C.TyConTetraTuple n
        S.PrimTyConTetraVector  -> C.TyConTetraVector
        S.PrimTyConTetraF       -> C.TyConTetraF
        S.PrimTyConTetraC       -> C.TyConTetraC
        S.PrimTyConTetraU       -> C.TyConTetraU


-- | Convert a value primtivie to a core name.
toCorePrimVal :: S.PrimVal -> C.Name
toCorePrimVal pv
 = case pv of
        S.PrimValArith  p       -> C.NamePrimArith  p False
        S.PrimValCast   p       -> C.NamePrimCast   p False
        S.PrimValError  p       -> C.NameOpError    p False
        S.PrimValVector p       -> C.NameOpVector   p False
        S.PrimValFun    p       -> C.NameOpFun      p
        S.PrimValLit    p       -> toCorePrimLit    p


-- | Convert a primitive literal to a core name.
toCorePrimLit :: S.PrimLit -> C.Name
toCorePrimLit pl
 = case pl of
        S.PrimLitBool    x      -> C.NameLitBool    x
        S.PrimLitNat     x      -> C.NameLitNat     x
        S.PrimLitInt     x      -> C.NameLitInt     x
        S.PrimLitSize    x      -> C.NameLitSize    x
        S.PrimLitWord    x s    -> C.NameLitWord    x s
        S.PrimLitFloat   x s    -> C.NameLitFloat   x s
        S.PrimLitChar    x      -> C.NameLitChar    x
        S.PrimLitTextLit x      -> C.NameLitTextLit x
