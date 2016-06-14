
-- | Definition of Source Tetra Expressions.
module DDC.Source.Tetra.Exp
        ( -- * Types
          -- ** Type Functions
          GTAnnot
        , GTBindVar, GTBoundVar
        , GTBindCon, GTBoundCon
        , GTPrim

          -- ** Abstract Syntax
        , GType       (..)
        , GTyCon      (..)
        , pattern TVoid
        , pattern TUnit
        , pattern TFun
        , pattern TBot
        , pattern TSum
        , pattern TForall
        , pattern TExists
        , pattern TPrim

          -- * Expressions
        , GXAnnot
        , GXBindVar, GXBoundVar
        , GXBindCon, GXBoundCon
        , GXPrim
        , GXBindVarMT   (..)
        , GExp          (..)
        , GLets         (..)
        , GAlt          (..)
        , GPat          (..)
        , GClause       (..)
        , GGuardedExp   (..)
        , GGuard        (..)
        , GCast         (..)
        , DaCon         (..)

        -- * Witnesses
        , GWitness      (..)
        , GWiCon        (..)

        -- * Dictionaries
        , ShowLanguage
        , NFDataLanguage)
where
import DDC.Type.Exp.Generic.Exp

import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.NFData

