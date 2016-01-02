
-- | Definition of Source Tetra Expressions.
module DDC.Source.Tetra.Exp
        ( module DDC.Type.Exp

        -- * Expressions
        , GName         (..)
        , GAnnot        (..)
        , GBind         (..)
        , GBound        (..)
        , GPrim         (..)
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
import DDC.Type.Exp
import DDC.Source.Tetra.Exp.Generic
