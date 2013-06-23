
module DDC.Core.Fragment.Feature
        (Feature(..))
where


-- | Language feature supported by a fragment.
data Feature
        -- Type system features ---------------------------
        -- | Assume all functions perform global side effects, 
        --   and don't generate effect terms in types.
        = UntrackedEffects

        -- | Assume all functions share data invisibly,
        --   and don't generate closure terms in types.
        | UntrackedClosures

        -- General features -------------------------------
        -- | Partially applied primitive operators.
        | PartialPrims

        -- | Partially applied functions
        | PartialApplication

        -- | Function application where the thing being applied
        --   is not a variable.
        --   Most backend languages (like LLVM) don't support this.
        | GeneralApplication

        -- | Nested function bindings.
        --   The output of the lambda-lifter should not contain these.
        | NestedFunctions

        -- | Debruijn binders.
        --   Most backends will want to use real names, instead of indexed
        --   binders.
        | DebruijnBinders

        -- | Allow data and witness vars without binding occurrences if
        --   they are annotated directly with their types. This lets
        --   us work with open terms.
        | UnboundLevel0Vars

        -- | Allow non-primitive functions to be instantiated at unboxed types.
        --   Our existing backends can't handle this, because boxed and unboxed
        --   objects have different representations.
        | UnboxedInstantiation

        -- Sanity -----------------------------------------
        -- | Allow name shadowing.
        | NameShadowing

        -- | Allow unused named data and witness bindings.
        | UnusedBindings

        -- | Allow unused named matches.
        | UnusedMatches
        deriving (Eq, Ord, Show)
