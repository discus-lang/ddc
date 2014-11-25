
module DDC.Core.Tetra.Compounds
        ( module DDC.Core.Compounds.Annot

          -- * Types
        , tBool, tNat, tInt, tWord, tPtr
        , tTupleN
        , tBoxed,       tUnboxed
        , tFunValue,    tCloValue

          -- * Expressions
        , xFunCReify,   xFunCCurry,    xFunApply, xFunCurry
        , xCastConvert)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim
import DDC.Core.Compounds.Annot
import DDC.Core.Exp


-- | Reify a super or foreign function into a closure.
xFunCReify
        :: a
        -> Type Name    -- ^ Parameter type.
        -> Type Name    -- ^ Result type.
        -> Exp a Name   -- ^ Input closure.
        -> Exp a Name   -- ^ Resulting closure.

xFunCReify a tParam tResult xF
 = xApps a
        (XVar a (UPrim  (NameOpFun OpFunCReify)
                        (typeOpFun OpFunCReify)))
        [XType a tParam, XType a tResult, xF]


-- | Construct a closure consisting of a top-level super and some arguments.
xFunCCurry
        :: a 
        -> [Type Name]  -- ^ Parameter types.
        -> Type Name    -- ^ Result type.
        -> Exp a Name   -- ^ Input closure.
        -> Exp a Name   -- ^ Resulting closure.

xFunCCurry a tsParam tResult xF
 = xApps a
         (XVar a (UPrim  (NameOpFun (OpFunCCurry (length tsParam)))
                         (typeOpFun (OpFunCCurry (length tsParam)))))
         ((map (XType a) tsParam) ++ [XType a tResult] ++ [xF])


-- | Construct a closure consisting of a top-level super and some arguments.
xFunCurry
        :: a 
        -> [Type Name]  -- ^ Parameter types.
        -> Type Name    -- ^ Result type.
        -> Exp a Name   -- ^ Input closure.
        -> Exp a Name   -- ^ Resulting closure.

xFunCurry a tsParam tResult xF
 = xApps a
         (XVar a (UPrim  (NameOpFun (OpFunCurry (length tsParam)))
                         (typeOpFun (OpFunCurry (length tsParam)))))
         ((map (XType a) tsParam) ++ [XType a tResult] ++ [xF])



-- | Apply a closure to more arguments.
xFunApply
        :: a 
        -> [Type Name]  -- ^ Argument types.
        -> Type Name    -- ^ Result type.
        -> Exp  a Name  -- ^ Functional expression.
        -> [Exp a Name] -- ^ Argument expressions.
        -> Exp a Name

xFunApply a tsArg tResult xF xsArg
 = xApps a
         (XVar a (UPrim  (NameOpFun (OpFunApply (length xsArg)))
                         (typeOpFun (OpFunApply (length xsArg)))))
         ((map (XType a) tsArg) ++ [XType a tResult] ++ [xF] ++ xsArg)


xCastConvert :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name 
xCastConvert a tTo tFrom x
 = xApps a
        (XVar a (UPrim (NamePrimCast PrimCastConvert) 
                       (typePrimCast PrimCastConvert)))
        [ XType a tTo
        , XType a tFrom
        , x ]

