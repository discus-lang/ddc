
module DDC.Core.Flow.Transform.Slurp.Error
        ( Error (..) )
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Transform.Annotate
import DDC.Core.Pretty


-- | Things that can go wrong when slurping a process spec from
--   Disciple Core Flow code.
data Error
        -- | Invalid series process definition.
        = ErrorBadProcess  (Exp () Name)

        -- | Invalid operator definition in process.
        | ErrorBadOperator (Exp () Name)

        -- | A series, process or resize is not bound locally,
        -- so is not in context
        | ErrorNotInContext Name

        -- | Cannot merge contexts
        -- TODO more info
        | ErrorCannotMergeContext

        -- | Cannot split contexts
        -- TODO more info
        | ErrorCannotSplitContext
        deriving Show


instance Pretty Error where
 ppr err
  = case err of
        ErrorBadProcess x
         -> vcat [ text "Bad series process definition."
                 , empty
                 , ppr (annotate () x) ]

        ErrorBadOperator x
         -> vcat [ text "Bad series operator."
                 , empty
                 , ppr (annotate () x) ]

        ErrorNotInContext n
         -> vcat [ text "Referenced name not in context."
                 , text "All Series, Processes and Resizes must be locally bound"
                 , empty
                 , ppr n ]

        ErrorCannotMergeContext
         -> vcat [ text "Cannot merge contexts" ]

        ErrorCannotSplitContext
         -> vcat [ text "Cannot split context into its append parts" ]

