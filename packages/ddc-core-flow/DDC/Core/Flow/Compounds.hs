
-- | Short-hands for constructing compound expressions.
module DDC.Core.Flow.Compounds
        ( module DDC.Core.Compounds.Simple

          -- * Fragment specific kinds
        , kRate

          -- * Fragment specific types
        , tTuple1, tTuple2, tTupleN
        , tVector, tSeries, tSegd, tSel1, tSel2, tRef, tWorld
        , tRateNat
        , tDown

          -- * Primtiive types
        , tVoid, tBool, tNat, tInt, tWord, tFloat, tVec
        , xGather, xScatter

          -- * Primitive literals and data constructors
        , xBool, dcBool
        , xNat,  dcNat
        ,          dcTuple1
        , xTuple2, dcTuple2
        , dcTupleN

          -- * Flow operators
        , xProj
        , xRateOfSeries
        , xNatOfRateNat

          -- * Loop operators
        , xLoopLoopN
        , xLoopGuard

          -- * Store operators
        , xNew,         xRead,       xWrite
        , xNewVector,   xNewVectorR, xNewVectorN
        , xReadVector,  xReadVectorC
        , xWriteVector, xWriteVectorC

        , xSliceVector
        , xNext,        xNextC)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConPrim
import DDC.Core.Flow.Prim.OpFlow
import DDC.Core.Flow.Prim.OpLoop
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Flow.Prim.OpPrim
import DDC.Core.Compounds.Simple
