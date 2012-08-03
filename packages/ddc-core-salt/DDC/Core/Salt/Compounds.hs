
module DDC.Core.Salt.Compounds
        ( tVoid
        , tBool
        , tNat, tInt, tWord
        , tTag
        , tObj
        , tAddr, tPtr
        , tString)
where
import DDC.Core.Salt.Name
import DDC.Type.Exp
import DDC.Type.Compounds


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
