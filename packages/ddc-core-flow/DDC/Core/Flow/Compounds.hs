
-- | Short-hands for constructing compound expressions.
module DDC.Core.Flow.Compounds
        ( module DDC.Core.Compounds.Simple

          -- * Fragment specific kinds
        , kRate
        , kProc

          -- * Fragment specific types
        , isRateNatType
        , isSeriesType
        , isRateVecType
        , isRefType
        , isVectorType
        , isProcessType
        , tTuple1, tTuple2, tTupleN
        , tVector, tBuffer, tSeries, tSegd, tSel1, tSel2, tRef, tWorld
        , tRateNat
        , tDown
        , tTail
        , tRatePlus, tRateTimes
        , tProcess

          -- * Primtiive types
        , tVoid, tBool, tNat, tInt, tWord, tFloat, tVec

          -- * Primitive literals and data constructors
        , xBool, dcBool
        , xNat,  dcNat
        ,          dcTuple1
        , xTuple2, dcTuple2
        , dcTupleN

          -- * Primitive Vec operators
        , xvRep
        , xvProj
        , xvGather
        , xvScatter

          -- * Series operators
        , xProj
        , xRateOfSeries
        , xNatOfRateNat
        , xNext, xNextC
        , xDown
        , xTail

          -- * Control operators
        , xLoopN
        , xGuard
        , xSegment
        , xSplit

          -- * Store operators
        , xNew,         xRead,       xWrite
        , xNewVector,   xNewVectorR, xNewVectorN
        , xReadVector,  xReadVectorC
        , xWriteVector, xWriteVectorC
        , xTailVector
        , xTruncVector)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConPrim
import DDC.Core.Flow.Prim.OpControl
import DDC.Core.Flow.Prim.OpConcrete
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Flow.Prim.OpPrim
import DDC.Core.Compounds.Simple
