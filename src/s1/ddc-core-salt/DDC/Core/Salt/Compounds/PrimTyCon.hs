
-- | Construct applications of primitive type constructors.
module DDC.Core.Salt.Compounds.PrimTyCon
        ( tVoid, tBool, tNat, tInt, tSize, tWord, tFloat
        , tAddr
        , tPtr,  takeTPtr
        , tTextLit
        , tTag

        , tObj)
where
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


tVoid, tBool, tNat, tInt, tSize, tTag, tAddr, tTextLit :: Type Name
tVoid     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid))    kData)
tBool     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool))    kData)
tNat      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat))     kData)
tInt      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt))     kData)
tSize     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConSize))    kData)
tAddr     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConAddr))    kData)
tTag      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTag))     kData)
tTextLit  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTextLit)) kData)


tWord :: Int -> Type Name
tWord bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits))) kData)


tFloat :: Int -> Type Name
tFloat bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConFloat bits))) kData)


-- Pointer
tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon tcPtr) r) t
 where  tcPtr   = TyConBound (UPrim (NamePrimTyCon PrimTyConPtr)) kPtr
        kPtr    = kRegion `kFun` kData `kFun` kData

takeTPtr :: Type Name -> Maybe (Region Name, Type Name)
takeTPtr tt
 = case tt of
        TApp (TApp (TCon tc) r) t
         | TyConBound (UPrim (NamePrimTyCon PrimTyConPtr)) _  <- tc
         -> Just (r, t)

        _ -> Nothing


tObj :: Type Name
tObj      = TCon (TyConBound (UPrim  NameObjTyCon) kData)

