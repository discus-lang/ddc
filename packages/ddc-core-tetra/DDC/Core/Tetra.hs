
module DDC.Core.Tetra
        ( -- * Language profile
          profile

          -- * Program Lexing
        , lexModuleString
        , lexExpString

          -- * Checking
        , checkModule

          -- * Conversion
        , saltOfTetraModule

          -- * Names
        , Name          (..)
        , TyConTetra    (..)
        , DaConTetra    (..)
        , OpFun         (..)
        , OpVector      (..)
        , PrimTyCon     (..),   pprPrimTyConStem
        , PrimArith     (..)

          -- * Name Parsing
        , readName
        , readTyConTetra
        , readDaConTetra
        , readOpFun
        , readOpVectorFlag
        , readPrimTyCon,        readPrimTyConStem
        , readPrimArith

        -- * Name Generation
        , freshT
        , freshX

        -- * Errors
        , Error(..))

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
import DDC.Core.Tetra.Convert   hiding (Error(..))
import DDC.Core.Tetra.Check
import DDC.Core.Tetra.Error
