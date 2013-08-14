
-- | Disciple Core Flow is a Domain Specific Language (DSL) for writing first
--   order data flow programs.
--   
module DDC.Core.Flow
        ( -- * Language profile
          profile

          -- * Driver
        , Config        (..)
        , defaultConfigVector
        , defaultConfigScalar
        , Method        (..)
        , lowerModule

          -- * Names
        , Name          (..)
        , KiConFlow     (..)
        , TyConFlow     (..)
        , DaConFlow     (..)
        , OpFlow        (..)
        , OpControl     (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimVec       (..)
        , PrimCast      (..)

          -- * Name Parsing
        , readName

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Lower
