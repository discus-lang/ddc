
module DDC.Core.Flow.Compounds
        ( module DDC.Core.Compounds

          -- * KiConFlow
        , kNatP, kRate

          -- * TyConFlow
        , tTuple2, tVector, tSeries, tSegd, tSel1, tSel2, tRef, tWorld
        , tRateNat

          -- * TyConPrim
        , tVoid, tBool, tNat, tInt, tWord

          -- * DaConPrim
        , xNat,  dcNat
        ,          dcTuple1
        , xTuple2, dcTuple2

          -- * OpFlow
        , xRateOfStream

          -- * OpLoop
        , xLoopLoopN

          -- * OpStore
        , xNew,       xRead,       xWrite
        , xNewVector, xReadVector, xWriteVector
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConPrim
import DDC.Core.Flow.Prim.OpFlow
import DDC.Core.Flow.Prim.OpLoop
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Compounds
