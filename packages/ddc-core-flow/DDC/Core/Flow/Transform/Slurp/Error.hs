
module DDC.Core.Flow.Transform.Slurp.Error
        (Error (..))
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Transform.Annotate
import DDC.Core.Pretty


-- | Things that can go wrong when slurping a process spec from
--   Disciple Core Flow code.
data Error
        -- | Invalid flow process definition.
        = ErrorBadProcess  (Exp () Name)

        -- | Invalid operator definition in process.
        | ErrorBadOperator (Exp () Name)
        deriving Show


instance Pretty Error where
 ppr err
  = case err of
        ErrorBadProcess x
         -> vcat [ text "Bad flow process definition."
                 , empty
                 , ppr (annotate () x) ]

        ErrorBadOperator x
         -> vcat [ text "Bad flow operator."
                 , empty
                 , ppr (annotate () x) ]
