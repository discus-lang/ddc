
module DDC.Core.Llvm.Convert.Context
        ( Context (..)
        , topEnvOfContext
        , takeVarOfContext
        , takeNonVoidVarOfContext
        , coreModuleOfContext)
where
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Llvm.Syntax
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Module        as C


-- | What context we're doing this conversion in.
data Context
        -- | Conversion at the top-level of a function.
        --   The expresison being converted must eventually pass control.
        --   The top-level context contains the original core module, and top-level kind 
        --   env type environments which we use to decide whether a variable has local or
        --   global scope.
        = ContextTop 
                (C.Module () A.Name)
                (KindEnv A.Name) 
                (TypeEnv A.Name)

        -- | In a nested context, like in the right of a let-binding.
        --   The expression should produce a value that we assign to this
        --   variable, then jump to the provided label to continue evaluation.
        | ContextNest Context Var Label

        -- | In a nested context where we need to assign the result
        --   to the given variable and fall through.
        | ContextAssign Context Var


-- | Get the core module from a `Context`.
coreModuleOfContext :: Context -> C.Module () A.Name
coreModuleOfContext context
 = case context of
        ContextTop mm _ _          -> mm
        ContextNest   context' _ _ -> coreModuleOfContext context'
        ContextAssign context' _   -> coreModuleOfContext context'


-- | Get the top-level environment from a `Context`.
topEnvOfContext :: Context -> (KindEnv A.Name, TypeEnv A.Name)
topEnvOfContext context
 = case context of
        ContextTop    _ kenv tenv  -> (kenv, tenv)
        ContextNest   context' _ _ -> topEnvOfContext context'
        ContextAssign context' _   -> topEnvOfContext context'


-- | Take any assignable variable from a `Context`.
takeVarOfContext :: Context -> Maybe Var
takeVarOfContext context
 = case context of
        ContextTop _ _ _           -> Nothing
        ContextNest _ var _        -> Just var
        ContextAssign _ var        -> Just var


-- | Take any assignable variable from a `Context`, but only if it has a non-void type.
--   In LLVM we can't assign to void variables.
takeNonVoidVarOfContext :: Context -> Maybe Var
takeNonVoidVarOfContext context
 = case takeVarOfContext context of
        Just (Var _ TVoid)       -> Nothing
        mv                       -> mv
