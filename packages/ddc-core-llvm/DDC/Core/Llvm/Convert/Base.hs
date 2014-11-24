
module DDC.Core.Llvm.Convert.Base
        ( Context       (..)
        , extendKindEnv, extendsKindEnv
        , extendTypeEnv, extendsTypeEnv

        , ExpContext    (..)
        , takeVarOfContext
        , takeNonVoidVarOfContext)
where
import DDC.Core.Salt.Platform
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Type.Exp
import DDC.Llvm.Syntax
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Module        as C
import qualified DDC.Type.Env           as Env


---------------------------------------------------------------------------------------------------
-- | Context of an Salt to LLVM conversion.
data Context 
        = Context
        { -- | The platform that we're converting to, 
          --   this sets the pointer width.
          contextPlatform       :: Platform

          -- | Surrounding module.
        , contextModule         :: C.Module () A.Name

          -- | The top-level kind environment.
        , contextKindEnvTop     :: KindEnv  A.Name

          -- | The top-level type environment.
        , contextTypeEnvTop     :: TypeEnv  A.Name

          -- | Current kind environment.
        , contextKindEnv        :: KindEnv  A.Name

          -- | Current type environment.
        , contextTypeEnv        :: TypeEnv  A.Name 

          -- | Super meta data
        , contextMDSuper        :: MDSuper }


-- | Extend the kind environment of a context with a new binding.
extendKindEnv  :: Bind A.Name  -> Context -> Context
extendKindEnv b ctx
        = ctx { contextKindEnv = Env.extend b (contextKindEnv ctx) }


-- | Extend the kind environment of a context with some new bindings.
extendsKindEnv :: [Bind A.Name] -> Context -> Context
extendsKindEnv bs ctx
        = ctx { contextKindEnv = Env.extends bs (contextKindEnv ctx) }


-- | Extend the type environment of a context with a new binding.
extendTypeEnv  :: Bind A.Name   -> Context -> Context
extendTypeEnv b ctx
        = ctx { contextTypeEnv = Env.extend b (contextTypeEnv ctx) }


-- | Extend the type environment of a context with some new bindings.
extendsTypeEnv :: [Bind A.Name] -> Context -> Context
extendsTypeEnv bs ctx
        = ctx { contextTypeEnv = Env.extends bs (contextTypeEnv ctx) }


---------------------------------------------------------------------------------------------------
-- | What expression context we're doing this conversion in.
data ExpContext
        -- | Conversion at the top-level of a function.
        --   The expresison being converted must eventually pass control.
        = ExpTop 

        -- | In a nested context, like in the right of a let-binding.
        --   The expression should produce a value that we assign to this
        --   variable, then jump to the provided label to continue evaluation.
        | ExpNest   ExpContext Var Label

        -- | In a nested context where we need to assign the result
        --   to the given variable and fall through.
        | ExpAssign ExpContext Var


-- | Take any assignable variable from a `Context`.
takeVarOfContext :: ExpContext -> Maybe Var
takeVarOfContext context
 = case context of
        ExpTop                  -> Nothing
        ExpNest _ var _         -> Just var
        ExpAssign _ var         -> Just var


-- | Take any assignable variable from a `Context`, but only if it has a non-void type.
--   In LLVM we can't assign to void variables.
takeNonVoidVarOfContext :: ExpContext -> Maybe Var
takeNonVoidVarOfContext context
 = case takeVarOfContext context of
        Just (Var _ TVoid)       -> Nothing
        mv                       -> mv

