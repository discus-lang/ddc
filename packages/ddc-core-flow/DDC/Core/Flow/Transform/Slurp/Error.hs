
module DDC.Core.Flow.Transform.Slurp.Error
        ( Error (..) )
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Transform.Annotate
import DDC.Core.Pretty
import DDC.Core.Flow.Context
import DDC.Core.Flow.Process.Pretty ()


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
        | ErrorCannotMergeContext Context Context

        -- | Cannot split contexts
        -- TODO more info
        | ErrorCannotSplitContext Context

        -- | Cannot resize a non-append
        | ErrorCannotResizeContext Context
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

        ErrorCannotMergeContext c1 c2
         -> vcat [ text "Cannot merge contexts"
                 , empty
                 , text "Embed:"
                 , ppr c1
                 , empty
                 , text "Into:"
                 , ppr c2 ]

        ErrorCannotSplitContext c
         -> vcat [ text "Cannot split context into its append parts"
                 , empty
                 , ppr c]

        ErrorCannotResizeContext c
         -> vcat [ text "Cannot resize append context, because it's not an append"
                 , empty
                 , ppr c]

