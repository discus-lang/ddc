{-# LANGUAGE OverloadedStrings #-}

-- | Construct applications of primitive store operators.
module DDC.Core.Salt.Compounds.PrimStore
        ( rTop
        , ukTop

        , xStoreSize, xStoreSize2
        , xRead, xWrite
        , xPeek, xPoke
        , xPeekBounded, xPokeBounded
        , xPlusPtr
        , xCastPtr

        , typeOfPrimStore)
where
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


-- Regions --------------------------------------------------------------------
-- | The top-level region.
--   This region lives for the whole program, and is used to store objects whose
--   types don't have region annotations (like function closures and Unit values).
rTop    :: Type Name
rTop   = TVar (fst ukTop)

ukTop :: (Bound Name, Kind Name)
ukTop
 =      ( UName (NameVar "rT")
        , kRegion)


-- | All the Prim Store vars have this form.
xPrimStore a p
 = XVar a (UPrim (NamePrimOp $ PrimStore p))


-- | Take the number of bytes needed to store a value of a primitive type.
xStoreSize :: a -> Type Name  -> Exp a Name
xStoreSize a tElem
 = xApps a      (xPrimStore a PrimStoreSize)
                [RType tElem]


-- | Log2 of the number of bytes needed to store a value of primitive type.
xStoreSize2 :: a -> Type Name  -> Exp a Name
xStoreSize2 a tElem
 = xApps a      (xPrimStore a PrimStoreSize2)
                [RType tElem]


-- | Read a value from an address plus offset.
xRead   :: a -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name
xRead a tField xAddr xOffset
 = xApps a      (xPrimStore a PrimStoreRead)
                [ RType tField, RTerm  xAddr, RTerm xOffset ]


-- | Write a value to an address plus offset.
xWrite   :: a -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xWrite a tField xAddr xOffset xVal
 = xApps a      (xPrimStore a PrimStoreWrite)
                [ RType tField, RTerm xAddr, RTerm xOffset, RTerm xVal ]


-- | Peek a value from a buffer pointer plus offset.
xPeek :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xPeek a r t xPtr
 = xApps a      (xPrimStore a PrimStorePeek)
                [ RType r, RType t, RTerm xPtr ]


-- | Poke a value from a buffer pointer plus offset.
xPoke   :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name
xPoke a r t xPtr xVal
 = xApps a      (xPrimStore a PrimStorePoke)
                [ RType r, RType t, RTerm xPtr, RTerm xVal]


-- | Peek a value from a buffer pointer plus offset.
xPeekBounded
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xPeekBounded a r t xPtr xOffset xLimit
 = xApps a      (xPrimStore a PrimStorePeekBounded)
                [ RType r, RType t, RTerm xPtr, RTerm xOffset, RTerm xLimit ]


-- | Poke a value from a buffer pointer plus offset.
xPokeBounded
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xPokeBounded a r t xPtr xOffset xLimit xVal
 = xApps a      (xPrimStore a PrimStorePokeBounded)
                [ RType r, RType t, RTerm xPtr, RTerm xOffset, RTerm xLimit, RTerm xVal]


-- | Add a byte offset to a pointer.
xPlusPtr :: a -> Type Name -> Type Name
         -> Exp a Name -> Exp a Name -> Exp a Name
xPlusPtr a r t xPtr xOffset
 = xApps a      (xPrimStore a PrimStorePlusPtr)
                [ RType r, RType t, RTerm xPtr, RTerm xOffset ]


-- | Cast a pointer to a different element ype.
xCastPtr :: a -> Type Name -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xCastPtr a r toType fromType xPtr
 = xApps a      (xPrimStore a PrimStoreCastPtr)
                [ RType r, RType toType, RType fromType, RTerm xPtr ]


-- | Take the type of a primitive projection.
typeOfPrimStore :: PrimStore -> Type Name
typeOfPrimStore jj
 = case jj of
        PrimStoreSize
         -> tForall kData $ \_ -> tNat

        PrimStoreSize2
         -> tForall kData $ \_ -> tNat

        PrimStoreCheck
         -> tNat `tFun` tBool

        PrimStoreRecover
         -> tNat `tFun` tVoid

        PrimStoreAlloc
         -> tNat `tFun` tAddr

        PrimStoreAllocSlot
         -> tForall kRegion
         $  \r -> tPtr rTop (tPtr r tObj)

        PrimStoreAllocSlotVal
         -> tForall kRegion
         $  \r -> tPtr r tObj `tFun` tPtr rTop (tPtr r tObj)

        PrimStoreRead
         -> tForall kData
         $ \t -> tAddr `tFun` tNat `tFun` t

        PrimStoreWrite
         -> tForall kData
         $ \t -> tAddr `tFun` tNat `tFun` t `tFun` tVoid

        PrimStoreCopy
         -> tAddr  `tFun` tAddr `tFun` tNat `tFun` tVoid

        PrimStoreSet
         -> tAddr  `tFun` tWord 8 `tFun` tNat `tFun` tVoid

        PrimStorePlusAddr
         -> tAddr  `tFun` tNat `tFun` tAddr

        PrimStoreMinusAddr
         -> tAddr  `tFun` tNat `tFun` tAddr

        PrimStorePeek
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` t

        PrimStorePoke
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` t `tFun` tVoid

        PrimStorePeekBounded
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` tNat `tFun` t

        PrimStorePokeBounded
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` tNat `tFun` t `tFun` tVoid

        PrimStorePlusPtr
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` tPtr r t

        PrimStoreMinusPtr
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` tPtr r t

        PrimStoreMakePtr
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tAddr `tFun` tPtr r t

        PrimStoreTakePtr
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tAddr

        PrimStoreCastPtr
         -> tForalls [kRegion, kData, kData]
         $ \[r,t1,t2] -> tPtr r t2 `tFun` tPtr r t1

        PrimStoreRootChain      -> tAddr

        PrimStoreGlobal
         -> tForall kData
         $ \_t -> tTextLit `tFun` tAddr

