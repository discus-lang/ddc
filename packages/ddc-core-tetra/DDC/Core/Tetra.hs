
module DDC.Core.Tetra
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConPrim     (..)
        , PrimArith     (..)
        , PrimRef       (..)

          -- * Name Parsing
        , readName
        , readPrimArith
        , readPrimRef
        , readTyConPrim

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
