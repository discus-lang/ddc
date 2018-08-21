
module DDC.Core.Salt.Compounds.Lit
        ( xBool
        , xNat,  xInt, xSize
        , xWord, xFloat, xTag
        , xChar, xTextLit)
where
import DDC.Core.Salt.Name
import DDC.Core.Exp
import Data.Text                        (Text)

xBool :: a -> Bool   -> Exp a Name
xBool a b       = XCon a (DaConPrim (NameLitBool b))

xNat  :: a -> Integer -> Exp a Name
xNat a i        = XCon a (DaConPrim (NameLitNat i))

xInt  :: a -> Integer -> Exp a Name
xInt a i        = XCon a (DaConPrim (NameLitInt i))

xSize :: a -> Integer -> Exp a Name
xSize a i       = XCon a (DaConPrim (NameLitSize i))

xWord :: a -> Integer -> Int -> Exp a Name
xWord a i bits  = XCon a (DaConPrim (NameLitWord i bits))

xFloat :: a -> Double -> Int -> Exp a Name
xFloat a i bits = XCon a (DaConPrim (NameLitFloat i bits))

xTag  :: a -> Integer -> Exp a Name
xTag a i        = XCon a (DaConPrim (NameLitTag i))

xChar    :: a -> Char -> Exp a Name
xChar    a c    = XCon a (DaConPrim (NameLitChar c))

xTextLit :: a -> Text -> Exp a Name
xTextLit a tx   = XCon a (DaConPrim (NameLitTextLit tx))
