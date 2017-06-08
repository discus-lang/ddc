
module DDC.Core.Tetra.Compounds
        ( module DDC.Core.Exp.Annot

          -- * Primitive
        , tVoid, tBool, tNat, tInt, tSize, tWord, tFloat
        , tPtr

          -- * Tetra types.
        , tTupleN
        , tUnboxed
        , tFunValue,    tCloValue
        , tTextLit

          -- * Expressions
        , xFunCReify,   xFunCCurry,    xFunApply, xFunCurry
        , xCastConvert)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.OpCast
import DDC.Core.Tetra.Prim.OpFun
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Exp.Annot


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
        [RType tParam, RType tResult, RTerm xF]


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
         ((map RType tsParam) ++ [RType tResult] ++ [RTerm xF])


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
         ((map RType tsParam) ++ [RType tResult] ++ [RTerm xF])



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
         ((map RType tsArg) ++ [RType tResult] ++ [RTerm xF] ++ (map RTerm xsArg))


xCastConvert :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name 
xCastConvert a tTo tFrom x
 = xApps a
        (XVar a (UPrim (NamePrimCast PrimCastConvert False) 
                       (typePrimCastFlag PrimCastConvert False)))
        [ RType tTo, RType tFrom
        , RTerm x ]

