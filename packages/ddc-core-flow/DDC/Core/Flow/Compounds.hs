
-- | Short-hands for constructing compound expressions.
module DDC.Core.Flow.Compounds
        ( module DDC.Core.Compounds.Simple

          -- * Fragment specific kinds
        , kRate

          -- * Fragment specific types
        , tTuple1, tTuple2, tTupleN
        , tVector, tSeries, tSegd, tSel1, tSel2, tRef, tWorld
        , tRateNat

          -- * Primtiive types
        , tVoid, tBool, tNat, tInt, tWord

          -- * Primitive literals and data constructors
        , xBool, dcBool
        , xNat,  dcNat
        ,          dcTuple1
        , xTuple2, dcTuple2
        , dcTupleN

          -- * Flow operators
        , xRateOfSeries
        , xNatOfRateNat

          -- * Loop operators
        , xLoopLoopN
        , xLoopGuard

          -- * Store operators
        , xNew,       xRead,       xWrite
        , xNewVector, xReadVector, xWriteVector, xNewVectorR, xNewVectorN
        , xSliceVector
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConPrim
import DDC.Core.Flow.Prim.OpFlow
import DDC.Core.Flow.Prim.OpLoop
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Compounds.Simple
