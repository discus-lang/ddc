
module DDC.Core.Flow.Transform.Schedule.Fail
        (Fail (..))
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim


-- | Reason a process kernel could not be scheduled into a procedure.
data Fail
        -- | The return type of a kernel must be Unit, but it wasn't.
        = FailReturnTypeNotUnit
        { failReturnType        :: Type Name }

        -- | Process has no rate parameters.
        | FailNoRateParameters

        -- | Process has no series parameters, 
        --   but there needs to be at least one.
        | FailNoSeriesParameters

        -- | Process has series of different rates,
        --   but all series must have the same rate.
        | FailMultipleRates

        -- | Primary rate variable of the process does not match
        --   the rate of the paramter series.
        | FailPrimaryRateMismatch

        -- | Cannot lift expression to vector operators.
        | FailCannotLift (Exp () Name)
        deriving Show

