
module DDC.Core.Language.Feature
        (Feature(..))
where

-- | Language Features
data Feature

        -- General features -------------------------------
        -- | Partial application of functions.
        --   Turning this off means the runtime system won't need to build PAPs.
        = PartialApplication

        -- | Accept partially applied primops.
        --   Runtime systems usually can't handle this.
        | PartialPrims

        -- | Allow function application between non-variables.
        | GeneralApplication

        -- | Nested function bindings.
        --   The output of the lambda-lifter should not contain nested functions.
        | NestedFunctions

        -- | Lazy let-bindings.
        --   Turning this off means the runtime system won't need to build
        --   suspensions.
        | LazyBindings

        -- | Data constructors.
        --   Turning this off, along with partial application, prevents heap
        --   allocation other than that performed by primitive operators.
        | DataCtors

        -- | Debruijn binders
        --   Most backends will want to use real names, instead of indexed binders.
        | DebruijnBinders

        -- Regions ----------------------------------------
        -- | The let-region construct.
        --   Turning this off forces all regions to be pre-allocated. 
        | LetRegion

        -- | Mutable regions.
        --   Turning this off forces all regions to be constant.
        | MutableRegions

        -- | Local regions.
        --   Turning this off forces all regions to be global.
        | LocalRegions

        -- | Global regions.
        --   Turning this off forces all regions to be local.
        | GlobalRegions

        -- Foreign modules --------------------------------
        -- | Imports.
        --   Turning this off means the module can only use primops,
        --   and the functions it itself defines.
        | Imports

        -- Sanity -----------------------------------------
        -- | Allow name shadowing.
        | NameShadowing

        -- | Allow unused named bindings.
        | UnusedBindings

        -- | Allow unused named matches.
        | UnusedMatches

        -- | Allow unsued imports.
        | UnusedImports
        deriving (Eq, Ord, Show)
