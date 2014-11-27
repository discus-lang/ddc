
module DDC.Core.Flow.Transform.Schedule.Error
        (Error (..))
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Process.Operator
import DDC.Core.Transform.Annotate
import DDC.Core.Pretty
import qualified DDC.Core.Flow.Transform.Slurp.Error    as Slurp


-- | Reason a process kernel could not be scheduled into a procedure.
data Error
        -- | Process has no rate parameters.
        = ErrorNoRateParameters

        -- | Process has series of different rates,
        --   but all series must have the same rate.
        | ErrorMultipleRates

        -- | Primary rate variable of the process does not match
        --   the rate of the paramter series.
        | ErrorPrimaryRateMismatch

        -- | Cannot lift expression to vector operators.
        | ErrorCannotLiftExp  (Exp () Name)

        -- | Cannot lift type to vector type.
        | ErrorCannotLiftType (Type Name)

        -- | Current scheduler does not support this operator.
        | ErrorUnsupported Operator

        -- | Multiple fills to the same output, in "interfering contexts" (eg same branch of an append)
        | ErrorMultipleFills

        -- | Cannot slurp process description from one of the top-level
        --   declarations.
        | ErrorSlurpError Slurp.Error
        deriving Show


instance Pretty Error where
 ppr err
  = case err of
        ErrorNoRateParameters
         -> vcat [ text "Series process has no rate parameters." ]

        ErrorMultipleRates
         -> vcat [ text "Series process has multiple rate parameters."]

        ErrorPrimaryRateMismatch
         -> vcat [ text "Series process primary rate mismatch."]

        ErrorCannotLiftExp x
         -> vcat [ text "Cannot lift expression in series process."
                 , empty
                 , indent 4 $ ppr (annotate () x) ]

        ErrorCannotLiftType t
         -> vcat [ text "Cannot lift type in series process."
                 , empty
                 , indent 4 $ ppr t ]

        ErrorUnsupported _
         -> vcat [ text "Cannot lower series operator with this method."]

        ErrorMultipleFills
         -> vcat [ text "Multiple fills to the same output, in 'interfering contexts' (eg same branch of an append)" ]

        ErrorSlurpError errSlurp
         -> vcat [ text "Error slurping series process."
                 , indent 2 $ ppr errSlurp ]

