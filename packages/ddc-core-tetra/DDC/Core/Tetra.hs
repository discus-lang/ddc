
module DDC.Core.Tetra
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConData     (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..)

          -- * Name Parsing
        , readName
        , readTyConData
        , readOpStore
        , readPrimTyCon
        , readPrimArith

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
