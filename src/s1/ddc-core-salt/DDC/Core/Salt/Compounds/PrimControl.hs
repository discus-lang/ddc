
-- | Construct applications of primitive control operators.
module DDC.Core.Salt.Compounds.PrimControl
        ( xFail
        , xReturn
        , typeOfPrimControl)
where
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


-- | All the Prim Control vars have this form.
xPrimControl a p
 = XVar a (UName (NamePrimOp $ PrimControl p))


-- | Fail with an internal error.
xFail   :: a -> Type Name -> Exp a Name
xFail a t
 = xApps a      (xPrimControl a PrimControlFail)
                [RType t]


-- | Return a value.
xReturn :: a -> Type Name -> Exp a Name -> Exp a Name
xReturn a t x
 = xApps a      (xPrimControl a PrimControlReturn)
                [RType t, RTerm x]


-- | Produce the type of a primitive control operator.
typeOfPrimControl :: PrimControl -> Type Name
typeOfPrimControl pc
 = case pc of
        PrimControlFail         -> tForall kData $ \t -> t
        PrimControlReturn       -> tForall kData $ \t -> t `tFun` t

        PrimControlCall arity
         -> let tSuper   = foldr tFun
                                 (tPtr (TVar (UIx 0)) tObj)
                                 (reverse [tPtr (TVar (UIx i)) tObj | i <- [1..arity]])

                tCall    = foldr TForall (tAddr `tFun` tSuper)
                                 [BAnon k | k <- replicate (arity + 1) kRegion]

           in   tCall

        PrimControlTailCall arity
         -> let tSuper   = foldr tFun
                                 (TVar (UIx 0))
                                 (reverse [TVar (UIx i) | i <- [1..arity]])

                tCall    = foldr TForall (tSuper `tFun` tSuper)
                                 [BAnon k | k <- replicate (arity + 1) kData]

           in   tCall

