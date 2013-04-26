
module DDC.Core.Flow.Compounds
        ( module DDC.Core.Compounds

          -- * KiConFlow
        , kNatP, kRate

          -- * TyConFlow
        , tLen, tTuple2, tArray, tVector, tStream, tSegd, tSel1, tSel2, tRef, tWorld

          -- * TyConPrim
        , tVoid, tBool, tNat, tInt, tWord

          -- * DaConPrim
        , xNat,  dcNat, xTuple2, dcTuple2

          -- * OpFlow
        , xLengthOfRate

          -- * OpStore
        , xNew,      xRead,      xWrite
        , xNewArray, xReadArray, xWriteArray
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConPrim
import DDC.Core.Flow.Prim.OpFlow
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Compounds
