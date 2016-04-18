
-- | Construct applications of primitive store operators.
module DDC.Core.Salt.Compounds.PrimStore
        ( xStoreSize, xStoreSize2
        , xCreate
        , xRead, xWrite
        , xPeek, xPeekBounded, xPoke, xPokeBounded
        , xCastPtr

        , typeOfPrimStore)
where
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


-- | All the Prim Store vars have this form.
xPrimStore a p
 = XVar a (UPrim (NamePrimOp $ PrimStore p) (typeOfPrimStore p))


-- | Take the number of bytes needed to store a value of a primitive type.
xStoreSize :: a -> Type Name  -> Exp a Name
xStoreSize a tElem
 = xApps a      (xPrimStore a PrimStoreSize) 
                [XType a tElem]


-- | Log2 of the number of bytes needed to store a value of primitive type.
xStoreSize2 :: a -> Type Name  -> Exp a Name
xStoreSize2 a tElem
 = xApps a      (xPrimStore a PrimStoreSize2) 
                [XType a tElem]

-- | Create the heap.
xCreate :: a -> Exp a Name -> Exp a Name
xCreate a xLength
 = xApps a      (xPrimStore a PrimStoreCreate)
                [xLength]


-- | Read a value from an address plus offset.
xRead   :: a -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name
xRead a tField xAddr xOffset
 = xApps a      (xPrimStore a PrimStoreRead)
                [ XType a tField, xAddr, xOffset ]


-- | Write a value to an address plus offset.
xWrite   :: a -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xWrite a tField xAddr xOffset xVal
 = xApps a      (xPrimStore a PrimStoreWrite)
                [ XType a tField, xAddr, xOffset, xVal ]
                

-- | Peek a value from a buffer pointer plus offset.
xPeek :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name
xPeek a r t xPtr xOffset
 = xApps a      (xPrimStore a PrimStorePeek)
                [ XType a r, XType a t, xPtr, xOffset ]


-- | Peek a value from a buffer pointer plus offset.
xPeekBounded 
        :: a -> Type Name -> Type Name 
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xPeekBounded a r t xPtr xOffset xLimit
 = xApps a      (xPrimStore a PrimStorePeekBounded)
                [ XType a r, XType a t, xPtr, xOffset, xLimit ]


-- | Poke a value from a buffer pointer plus offset.
xPoke   :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xPoke a r t xPtr xOffset xVal
 = xApps a      (xPrimStore a PrimStorePoke)
                [ XType a r, XType a t, xPtr, xOffset, xVal]


-- | Poke a value from a buffer pointer plus offset.
xPokeBounded
        :: a -> Type Name -> Type Name
        -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name -> Exp a Name
xPokeBounded a r t xPtr xOffset xLimit xVal
 = xApps a      (xPrimStore a PrimStorePokeBounded)
                [ XType a r, XType a t, xPtr, xOffset, xLimit, xVal]


-- | Cast a pointer to a different element ype.
xCastPtr :: a -> Type Name -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xCastPtr a r toType fromType xPtr
 = xApps a      (xPrimStore a PrimStoreCastPtr)
                [ XType a r, XType a toType, XType a fromType, xPtr ]


-- | Take the type of a primitive projection.
typeOfPrimStore :: PrimStore -> Type Name
typeOfPrimStore jj
 = case jj of
        PrimStoreSize 
         -> tForall kData $ \_ -> tNat

        PrimStoreSize2
         -> tForall kData $ \_ -> tNat

        PrimStoreCreate
         -> tNat `tFun` tVoid

        PrimStoreCheck
         -> tNat `tFun` tBool

        PrimStoreRecover
         -> tNat `tFun` tVoid

        PrimStoreAlloc
         -> tNat `tFun` tAddr

        PrimStoreRead           
         -> tForall kData 
         $ \t -> tAddr `tFun` tNat `tFun` t

        PrimStoreWrite
         -> tForall kData 
         $ \t -> tAddr `tFun` tNat `tFun` t `tFun` tVoid

        PrimStorePlusAddr
         -> tAddr  `tFun` tNat `tFun` tAddr

        PrimStoreMinusAddr
         -> tAddr  `tFun` tNat `tFun` tAddr

        PrimStorePeek
         -> tForalls [kRegion, kData]
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` t

        PrimStorePoke
         -> tForalls [kRegion, kData] 
         $ \[r,t] -> tPtr r t `tFun` tNat `tFun` t `tFun` tVoid

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

