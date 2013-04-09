
module DDC.Core.Flow.Compounds
        ( kNatP
        , kRate
        , tBoolU
        , tNatU,  dcNatU, xNatU
        , tIntU
        , tWordU

        , tLen
        , tTuple2
        , tArray, tVector, tStream
        , tSegd
        , tSel1, tSel2
        , tRef

        , xNew
        , xRead
        , xWrite
        , xNext
        , xLengthOfRate)
where
import DDC.Core.Flow.Name
import DDC.Core.Exp
import DDC.Core.DaCon
import DDC.Core.Compounds


-- Kind Constructors ----------------------------------------------------------
kNatP   = TCon (TyConBound (UPrim (NameFlowKiCon FlowKiConNatP) sProp) sProp)
kRate   = TCon (TyConBound (UPrim (NameFlowKiCon FlowKiConRate) sProp) sProp)


-- Unboxed Type Constructors --------------------------------------------------
-- | Unboxed `Bool#` type constructor.
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- | The Nat# type constructor.
tNatU ::  Type Name
tNatU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


-- | A Literal Nat# data constructor.
dcNatU :: Integer -> DaCon Name
dcNatU i = mkDaConAlg (NameLitNat i) tNatU


-- | A literal Nat#
xNatU  :: a -> Integer -> Exp a Name
xNatU a i = XCon a (dcNatU i)


-- | Unboxed `Int#` type constructor.
tIntU ::  Type Name
tIntU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)

-- | Unboxed `WordN#` type constructor of the given width.
tWordU :: Int -> Type Name
tWordU bits 
 = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


-- Flow DSL types -------------------------------------------------------------
-- Len
tLen    :: Type Name -> Type Name
tLen tN
 = tApp (TCon tcLen) tN
 where  uLen            = UPrim (NameFlowTyCon FlowTyConLen) kLen
        tcLen           = TyConBound uLen kLen
        kLen            = kNatP `kFun` kRate


tTuple2 :: Type Name -> Type Name -> Type Name
tTuple2 tA tB
 = tApps (TCon tcPair) [tA, tB]
 where  tcPair  = TyConBound (UPrim (NameDataTyCon (DataTyConTuple 2)) kPair) kPair
        kPair   = kFuns [kData, kData] kData


tArray :: Type Name -> Type Name
tArray tA
 = tApps (TCon tcArray) [tA]
 where  uArray         = UPrim (NameDataTyCon DataTyConArray) kArray
        tcArray        = TyConBound uArray kArray
        kArray         = kData `kFun` kData


tVector :: Type Name -> Type Name -> Type Name
tVector tK tA
 = tApps (TCon tcVector) [tK, tA]
 where  uVector         = UPrim (NameDataTyCon DataTyConVector) kVector
        tcVector        = TyConBound uVector kVector
        kVector         = kRate `kFun` kData `kFun` kData


tStream :: Type Name -> Type Name -> Type Name
tStream tK tA
 = tApps (TCon tcStream) [tK, tA]
 where  uStream         = UPrim (NameDataTyCon DataTyConStream) kStream
        tcStream        = TyConBound uStream kStream
        kStream         = kRate `kFun` kData `kFun` kData


tSegd :: Type Name -> Type Name -> Type Name
tSegd tK1 tK2 
 = tApps (TCon tcSegd) [tK1, tK2]
 where  uSegd         = UPrim (NameDataTyCon DataTyConSegd) kSegd
        tcSegd        = TyConBound uSegd kSegd
        kSegd         = kRate `kFun` kRate `kFun` kData


tSel1 :: Type Name -> Type Name -> Type Name
tSel1 tK1 tK2 
 = tApps (TCon tcSel1) [tK1, tK2]
 where  uSel1         = UPrim (NameDataTyCon $ DataTyConSel 1) kSel1
        tcSel1        = TyConBound uSel1 kSel1
        kSel1         = kRate `kFun` kRate `kFun` kData


tSel2 :: Type Name -> Type Name -> Type Name -> Type Name
tSel2 tK1 tK2 tK3 
 = tApps (TCon tcSel2) [tK1, tK2, tK3]
 where  uSel2         = UPrim (NameDataTyCon $ DataTyConSel 2) kSel2
        tcSel2        = TyConBound uSel2 kSel2
        kSel2         = kRate `kFun` kRate `kFun` kRate `kFun` kData


tRef  :: Type Name -> Type Name
tRef tVal
 = tApp (TCon tcRef) tVal
 where  uRef          = UPrim (NameDataTyCon DataTyConRef) kRef
        tcRef         = TyConBound uRef kRef
        kRef          = kData `kFun` kData


-- Operators ------------------------------------------------------------------
xNew :: Type Name -> Exp () Name -> Exp () Name
xNew t xV
 = xApps () (XVar () (UName (NameStoreOp StoreOpNew)))
            [XType t, xV ]


xRead :: Type Name -> Exp () Name -> Exp () Name
xRead t xRef
 = xApps () (XVar () (UName (NameStoreOp StoreOpRead)))
            [XType t, xRef ]


xWrite :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xWrite t xRef xVal
 = xApps () (XVar () (UName (NameStoreOp StoreOpWrite)))
            [XType t, xRef, xVal ]


xNext  :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext t xStream xIndex
 = xApps () (XVar () (UName (NameStoreOp StoreOpNext)))
            [XType t, xStream, xIndex]


xLengthOfRate :: Type Name -> Exp () Name
xLengthOfRate t
 = XApp () (XVar () $ UName (NameFlowOp FlowOpLengthOfRate)) 
           (XType t)

