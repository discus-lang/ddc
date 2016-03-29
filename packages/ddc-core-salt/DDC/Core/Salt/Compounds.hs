
module DDC.Core.Salt.Compounds
        ( -- * Types
          rTop,  ukTop
        , tVoid, tBool, tNat, tInt, tSize, tWord, tFloat
        , tAddr
        , tPtr,  takeTPtr
        , tTextLit
        , tTag
        , tObj

          -- * Literals
        , xBool,  xNat, xInt, xSize, xWord, xFloat, xTag, xTextLit

          -- * Prim Arith
        , xNeg
        , xAdd, xSub, xMul, xDiv, xMod, xRem
        , xEq,  xNeq, xLt,  xGt,  xLe,  xGe
        , xAnd, xOr
        , xShl, xShr, xBAnd, xBOr, xBXOr

          -- * Prim Cast
        , xConvert
        , xPromote
        , xTruncate

          -- * Prim Control
        , xFail
        , xReturn

          -- * Prim Store
        , xStoreSize, xStoreSize2
        , xCreate
        , xRead, xWrite
        , xPeek, xPeekBounded, xPoke, xPokeBounded
        , xCastPtr)
where
import DDC.Core.Salt.Compounds.PrimArith
import DDC.Core.Salt.Compounds.PrimCast
import DDC.Core.Salt.Compounds.PrimStore
import DDC.Core.Salt.Compounds.PrimControl
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp
import Data.Text                (Text)



-- Literals ----------------------------------------------------------------
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


