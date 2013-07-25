
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

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Tetra.Prim
import DDC.Core.Tetra.Profile
