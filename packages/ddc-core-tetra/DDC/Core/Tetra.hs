
module DDC.Core.Tetra
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConTetra    (..)
        , DaConTetra    (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..)

          -- * Name Parsing
        , readName
        , readTyConTetra
        , readDaConTetra
        , readOpStore
        , readPrimTyCon
        , readPrimArith

          -- * Program Lexing
        , lexModuleString
        , lexExpString

          -- * Conversion
        , saltOfTetraModule
        , Error         (..))

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
import DDC.Core.Tetra.Convert
