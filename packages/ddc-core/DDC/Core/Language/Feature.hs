
module DDC.Core.Language.Feature
        (Feature(..))
where

-- | Language Features
data Feature
        -- Primitive operations ---------------------------
        -- | Accept partially applied primops.
        --   Runtime systems usually can't handle this.
        = PartialPrims

        -- General features -------------------------------
        -- | Partial application of functions.
        --   Turning this off means the runtime system won't need to build PAPs.
        | PartialApplication

        -- | Nested function bindings.
        --   The output of the lambda-lifter should not contain nested functions.
        | NestedFunctions

        -- | Lazy let-bindings.
        --   Turning this off means the runtime system won't need to build
        --   suspensions.
        | LazyBindings

        -- | Data constructors.
        --  Turning this off, along with partial application, prevents heap
        --  allocation other than that performed by primitive operators.
        | DataCtors

        -- | Recursive let-bindings.
        --   Turning this off, along with mutable regions and conflicting primops,
        --   forces the language to total (terminating).
        | Letrec

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

        -- | Allow unused bindings.
        | UnusedBindings

        -- | Allow unused matches.
        | UnusedMatches

        -- | Allow unsued imports.
        | UnusedImports
        deriving (Eq, Ord, Show)
