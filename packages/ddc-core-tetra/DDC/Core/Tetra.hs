
module DDC.Core.Tetra
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConPrim     (..)
        , OpPrimArith   (..)
        , OpPrimRef     (..)

          -- * Name Parsing
        , readName
        , readOpPrimArith
        , readOpPrimRef
        , readTyConPrim

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
