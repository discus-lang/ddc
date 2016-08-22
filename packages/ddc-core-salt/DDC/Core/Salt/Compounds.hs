
module DDC.Core.Salt.Compounds
        ( -- * Types
          tVoid, tBool, tNat, tInt, tSize, tWord, tFloat
        , tAddr
        , tPtr,  takeTPtr
        , tTextLit
        , tTag
        , tObj

          -- * Values
          -- ** Literals
        , xBool, xNat, xInt, xSize, xWord, xFloat, xTag
        , xChar, xTextLit

          -- ** Primitive arithmetic operators.
        , xNeg
        , xAdd, xSub, xMul, xDiv, xMod, xRem
        , xEq,  xNeq, xLt,  xGt,  xLe,  xGe
        , xAnd, xOr
        , xShl, xShr, xBAnd, xBOr, xBXOr

          -- ** Primitive cast operators.
        , xConvert
        , xPromote
        , xTruncate

          -- ** Primitive control operators.
        , xFail
        , xReturn

          -- ** Primitive store operators.
        , xStoreSize, xStoreSize2
        , xCreate
        , xRead, xWrite
        , xPeek, xPeekBounded, xPoke, xPokeBounded
        , xCastPtr)
where
import DDC.Core.Salt.Compounds.Lit
import DDC.Core.Salt.Compounds.PrimArith
import DDC.Core.Salt.Compounds.PrimCast
import DDC.Core.Salt.Compounds.PrimStore
import DDC.Core.Salt.Compounds.PrimControl
import DDC.Core.Salt.Compounds.PrimTyCon


