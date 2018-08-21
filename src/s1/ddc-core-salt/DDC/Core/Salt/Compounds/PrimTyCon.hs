
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
tVoid           = TCon (TyConBound (NamePrimTyCon PrimTyConVoid))
tBool           = TCon (TyConBound (NamePrimTyCon PrimTyConBool))
tNat            = TCon (TyConBound (NamePrimTyCon PrimTyConNat))
tInt            = TCon (TyConBound (NamePrimTyCon PrimTyConInt))
tSize           = TCon (TyConBound (NamePrimTyCon PrimTyConSize))
tAddr           = TCon (TyConBound (NamePrimTyCon PrimTyConAddr))
tTag            = TCon (TyConBound (NamePrimTyCon PrimTyConTag))
tTextLit        = TCon (TyConBound (NamePrimTyCon PrimTyConTextLit))


tWord :: Int -> Type Name
tWord bits      = TCon (TyConBound (NamePrimTyCon (PrimTyConWord bits)))


tFloat :: Int -> Type Name
tFloat bits     = TCon (TyConBound (NamePrimTyCon (PrimTyConFloat bits)))

-- Pointer
tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon tcPtr) r) t
 where  tcPtr   = TyConBound (NamePrimTyCon PrimTyConPtr)

takeTPtr :: Type Name -> Maybe (Region Name, Type Name)
takeTPtr tt
 = case tt of
        TApp (TApp (TCon tc) r) t
         | TyConBound (NamePrimTyCon PrimTyConPtr) <- tc
         -> Just (r, t)

        _ -> Nothing


tObj :: Type Name
tObj  = TCon (TyConBound NameObjTyCon)

