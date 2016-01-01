
module DDC.Core.Salt.Compounds
        ( rTop,   ukTop
        , tVoid
        , tBool,  xBool
        , tNat,   xNat, dcNat
        , tInt,   xInt
        , tSize,  xSize
        , tWord,  xWord
        , tFloat, xFloat
        , tTag,   xTag
        , tObj
        , tAddr
        , tPtr,   takeTPtr
        , tTextLit
        , xTextLit)
where
import DDC.Core.Salt.Name
import DDC.Core.Exp
import DDC.Core.Compounds
import Data.Text                (Text)

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


-- Types ----------------------------------------------------------------------
tVoid, tBool, tNat, tInt, tSize, tTag, tAddr, tTextLit :: Type Name
tVoid     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid)    kData) kData)
tBool     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool)    kData) kData)
tNat      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat)     kData) kData)
tInt      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt)     kData) kData)
tSize     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConSize)    kData) kData)
tAddr     = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConAddr)    kData) kData)
tTag      = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTag)     kData) kData)
tTextLit  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConTextLit) kData) kData)


tWord :: Int -> Type Name
tWord bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


tFloat :: Int -> Type Name
tFloat bits = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConFloat bits)) kData) kData)


tObj :: Type Name
tObj      = TCon (TyConBound (UPrim  NameObjTyCon kData) kData)


tPtr :: Region Name -> Type Name -> Type Name
tPtr r t = TApp (TApp (TCon tcPtr) r) t
 where  tcPtr   = TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) kPtr) kPtr
        kPtr    = kRegion `kFun` kData `kFun` kData

takeTPtr :: Type Name -> Maybe (Region Name, Type Name)
takeTPtr tt
 = case tt of
        TApp (TApp (TCon tc) r) t
         | TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) _) _  <- tc
         -> Just (r, t)

        _ -> Nothing

-- Expressions ----------------------------------------------------------------
xBool :: a -> Bool   -> Exp a Name
xBool a b       = XCon a (DaConPrim (NameLitBool b) tBool)


xNat  :: a -> Integer -> Exp a Name
xNat a i        = XCon a (dcNat i)


xInt  :: a -> Integer -> Exp a Name
xInt a i        = XCon a (DaConPrim (NameLitInt i)  tInt)


xSize :: a -> Integer -> Exp a Name
xSize a i       = XCon a (DaConPrim (NameLitSize i) tSize)


xWord :: a -> Integer -> Int -> Exp a Name
xWord a i bits  = XCon a (DaConPrim (NameLitWord i bits) (tWord bits))


xFloat :: a -> Double -> Int -> Exp a Name
xFloat a i bits = XCon a (DaConPrim (NameLitFloat i bits) (tFloat bits))


xTag  :: a -> Integer -> Exp a Name
xTag a i        = XCon a (DaConPrim (NameLitTag i)  tTag)


-- | A Literal @Nat#@ data constructor.
dcNat   :: Integer -> DaCon Name
dcNat i         = DaConPrim (NameLitNat i) tNat


-- | A Text literal.
xTextLit :: a -> Text -> Exp a Name
xTextLit a tx    = XCon a (DaConPrim (NameLitTextLit tx) tTextLit)


