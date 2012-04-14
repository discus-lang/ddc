
-- | Primitive types and operators used by several
--   fragments of Disciple Core. 
module DDC.Core.Sea.Base
        ( PrimTyCon     (..)
        , readPrimTyCon

        , PrimOp        (..)
        , readPrimOp

        , readLitInteger
        , readLitPrimWordOfBits
        , readLitPrimIntOfBits)
where
import DDC.Core.Sea.Base.Name

