
module DDC.Core.Flow.Context
        (Context (..))
where
import DDC.Type.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Process.Operator

data Context
        -- | A top-level context associated with a rate that is a parameter
        --   of the process. This context isn't created by the process itself.
        = ContextRate
        { contextRate           :: Type Name
        , contextOps            :: [Operator]
        , contextInner          :: [Context] }

        -- | A nested context created by a mkSel1# function.
        | ContextSelect
        { contextOuterRate      :: Type  Name
        , contextInnerRate      :: Type  Name
        , contextFlags          :: Bound Name
        , contextSelector       :: Bind  Name
        , contextOps            :: [Operator]
        , contextInner          :: [Context] }


        -- | A nested context created by a mkSegd# function.
        | ContextSegment
        { contextOuterRate      :: Type  Name
        , contextInnerRate      :: Type  Name
        , contextLens           :: Bound Name
        , contextSegd           :: Bind  Name
        , contextOps            :: [Operator]
        , contextInner          :: [Context] }

        | ContextAppend
        { contextRate1          :: Type Name
        , contextInner1         :: Context
        , contextRate2          :: Type Name
        , contextInner2         :: Context }
        deriving Show

