
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

tVoid     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid)   kData))
tBool     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool)   kData))
tNat      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat)    kData))
tInt      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt)    kData))
tTag      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTag)    kData))
tAddr     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConAddr)   kData))
tString   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConString) kData))


tWord :: Int -> Type Name
tWord bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData))


tObj :: Type Name
tObj      = TCon (TyConBound (UPrim  NameObjTyCon kData))


tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) (kRegion `kFun` kData `kFun` kData))))
                      r)
                t
