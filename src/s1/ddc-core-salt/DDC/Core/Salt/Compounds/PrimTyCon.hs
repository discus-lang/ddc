
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
tVoid           = TCon (TyConBound (UName (NamePrimTyCon PrimTyConVoid))    kData)
tBool           = TCon (TyConBound (UName (NamePrimTyCon PrimTyConBool))    kData)
tNat            = TCon (TyConBound (UName (NamePrimTyCon PrimTyConNat))     kData)
tInt            = TCon (TyConBound (UName (NamePrimTyCon PrimTyConInt))     kData)
tSize           = TCon (TyConBound (UName (NamePrimTyCon PrimTyConSize))    kData)
tAddr           = TCon (TyConBound (UName (NamePrimTyCon PrimTyConAddr))    kData)
tTag            = TCon (TyConBound (UName (NamePrimTyCon PrimTyConTag))     kData)
tTextLit        = TCon (TyConBound (UName (NamePrimTyCon PrimTyConTextLit)) kData)


tWord :: Int -> Type Name
tWord bits      = TCon (TyConBound (UName (NamePrimTyCon (PrimTyConWord bits))) kData)


tFloat :: Int -> Type Name
tFloat bits     = TCon (TyConBound (UName (NamePrimTyCon (PrimTyConFloat bits))) kData)


-- Pointer
tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon tcPtr) r) t
 where  tcPtr   = TyConBound (UName (NamePrimTyCon PrimTyConPtr)) kPtr
        kPtr    = kRegion `kFun` kData `kFun` kData

takeTPtr :: Type Name -> Maybe (Region Name, Type Name)
takeTPtr tt
 = case tt of
        TApp (TApp (TCon tc) r) t
         | TyConBound (UName (NamePrimTyCon PrimTyConPtr)) _  <- tc
         -> Just (r, t)

        _ -> Nothing


tObj :: Type Name
tObj            = TCon (TyConBound (UName  NameObjTyCon) kData)

