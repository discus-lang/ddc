
module DDC.Core.Flow.Transform.Schedule
        ( scheduleProcess

         -- * Scheduling kernels
        , scheduleKernel
        , Fail          (..)
        , Lifting       (..))
where
import DDC.Core.Flow.Transform.Schedule.Kernel
import DDC.Core.Flow.Transform.Schedule.Scalar
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds


defaultLifting
        = Lifting
        { liftingFactor         = 8 }


-- | Create loops from a list of operators.
--
--   * The input series must all have the same rate.
--
scheduleProcess :: Process -> Procedure
scheduleProcess process
 | processResultType process == tUnit
 = case scheduleKernel defaultLifting process of
        Left  fails     -> error $ "scheduleProcess failed: " ++ show fails
        Right proc      -> proc

 | otherwise
 = scheduleScalar process





