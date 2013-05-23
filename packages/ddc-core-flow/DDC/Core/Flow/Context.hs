
module DDC.Core.Flow.Context
        (Context (..))
where
import DDC.Type.Exp
import DDC.Core.Flow.Prim

data Context
        -- | A top-level context associated with a rate that is a parameter
        --   of the process. This context isn't created by the process itself.
        = ContextRate
        { contextRate           :: Type Name }

        -- | A nested context created by a mkSel function.
        | ContextSelect
        { contextOuterRate      :: Type  Name
        , contextInnerRate      :: Type  Name
        , contextFlags          :: Bound Name
        , contextSelector       :: Bind  Name }
        deriving (Show, Eq)

