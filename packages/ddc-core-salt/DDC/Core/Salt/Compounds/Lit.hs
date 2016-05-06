
module DDC.Core.Salt.Compounds.Lit
        ( xBool
        , xNat, xInt, xSize
        , xWord, xFloat, xTag
        , xTextLit)
where
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp
import Data.Text                        (Text)


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
dcNat   :: Integer -> DaCon Name (Type Name)
dcNat i         = DaConPrim (NameLitNat i) tNat


-- | A Text literal.
xTextLit :: a -> Text -> Exp a Name
xTextLit a tx    = XCon a (DaConPrim (NameLitTextLit tx) tTextLit)
