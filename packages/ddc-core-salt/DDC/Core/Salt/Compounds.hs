
module DDC.Core.Salt.Compounds
        ( tVoid
        , tBool, xBool
        , tNat,  xNat
        , tInt,  xInt
        , tWord, xWord
        , tTag,  xTag
        , tObj
        , tAddr, tPtr
        , tString)
where
import DDC.Core.Salt.Name
import DDC.Core.Exp
import DDC.Type.Compounds


-- Types ----------------------------------------------------------------------
tVoid, tBool, tNat, tInt, tTag, tAddr, tString :: Type Name
tVoid     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid)   kData) kData)
tBool     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool)   kData) kData)
tNat      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat)    kData) kData)
tInt      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt)    kData) kData)
tTag      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTag)    kData) kData)
tAddr     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConAddr)   kData) kData)
tString   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConString) kData) kData)


tWord :: Int -> Type Name
tWord bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


tObj :: Type Name
tObj      = TCon (TyConBound (UPrim  NameObjTyCon kData) kData)


tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon tcPtr) r) t
 where  tcPtr   = TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) kPtr) kPtr
        kPtr    = kRegion `kFun` kData `kFun` kData


-- Expressions ----------------------------------------------------------------
xBool :: a -> Bool   -> Exp a Name
xBool a b       = XCon a (mkDaConAlg (NameBool b) tBool)


xNat  :: a -> Integer -> Exp a Name
xNat a i        = XCon a (mkDaConAlg (NameNat i) tNat)


xInt  :: a -> Integer -> Exp a Name
xInt a i        = XCon a (mkDaConAlg (NameInt i) tInt)


xWord :: a -> Integer -> Int -> Exp a Name
xWord a i bits  = XCon a (mkDaConAlg (NameWord i bits) (tWord bits))


xTag  :: a -> Integer -> Exp a Name
xTag a i        = XCon a (mkDaConAlg (NameTag i) tTag)

