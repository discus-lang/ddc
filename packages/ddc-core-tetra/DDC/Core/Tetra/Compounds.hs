
module DDC.Core.Tetra.Compounds
        ( module DDC.Core.Compounds.Annot

          -- * Types
        , tBool
        , tNat
        , tInt
        , tWord

        , tBoxed
        , tUnboxed

          -- * Expressions
        , xCastConvert)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim
import DDC.Core.Compounds.Annot
import DDC.Core.Exp



xCastConvert :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name 
xCastConvert a tTo tFrom x
        = xApps a
                (XVar a (UPrim (NamePrimCast PrimCastConvert) 
                               (typePrimCast PrimCastConvert)))
                [ XType a tTo
                , XType a tFrom
                , x ]

