
module DDC.Core.Fragment.Feature
        (Feature(..))
where

-- | Language Features
data Feature

        -- General features -------------------------------
        -- | Accept partially applied primops.
        --   Runtime systems usually can't handle this.
        = PartialPrims

        -- | Allow function application between non-variables.
        | GeneralApplication

        -- | Nested function bindings.
        --   The output of the lambda-lifter should not contain nested functions.
        | NestedFunctions

        -- | Lazy let-bindings.
        --   Turning this off means the runtime system won't need to build
        --   suspensions.
        | LazyBindings

        -- | Debruijn binders
        --   Most backends will want to use real names, instead of indexed binders.
        | DebruijnBinders

        -- Sanity -----------------------------------------
        -- | Allow name shadowing.
        | NameShadowing

        -- | Allow unused named data and witness bindings.
        | UnusedBindings

        -- | Allow unused named matches.
        | UnusedMatches
        deriving (Eq, Ord, Show)
