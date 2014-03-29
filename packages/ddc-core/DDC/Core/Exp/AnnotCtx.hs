
module DDC.Core.Exp.AnnotCtx   
        ( Ctx (..)
        , takeEnclosingCtx)
where
import DDC.Core.Exp.Annot


data Ctx a n
        -- | The top-level context.
        = CtxTop

        -- | Body of a type abstraction.
        | CtxLAM        !(Ctx a n) !a
                        !(Bind n)
        
        -- | Body of a value or witness abstraction.
        | CtxLam        !(Ctx a n) !a
                        !(Bind n)

        -- | Left of an application.
        | CtxAppLeft    !(Ctx a n) !a
                        !(Exp a n)

        -- | Right of an application.
        | CtxAppRight   !(Ctx a n) !a
                        !(Exp a n)

        -- | Body of a let-expression.
        | CtxLetBody    !(Ctx a n) !a
                        !(Lets a n)

        -- | In a non-recursive let-binding.
        --   We store the binder and body of the let expression.
        | CtxLetLLet    !(Ctx a n) !a
                        !(Bind n)       -- binder of current let-binding.
                        !(Exp a n)      -- let body

        -- | In a recursive binding.
        | CtxLetLRec    !(Ctx a n) !a
                        ![(Bind n, Exp a n)] !(Bind n) ![(Bind n, Exp a n)]
                        !(Exp a n)

        -- | Scrutinee of a case expression.
        | CtxCaseScrut  !(Ctx a n) !a
                        ![Alt a n]

        -- | In a case alternative.
        | CtxCaseAlt    !(Ctx a n) !a
                        !(Exp a n)      -- case scrutinee
                        ![Alt a n] !(Pat n) ![Alt a n]

        -- | Body of a type cast
        | CtxCast       !(Ctx a n) !a   -- context of let-expression.
                        !(Cast a n)
        deriving (Show, Eq)


-- | Take the enclosing context from a nested one.
takeEnclosingCtx :: Ctx a n -> Maybe (Ctx a n)
takeEnclosingCtx ctx
 = case ctx of
        CtxTop                   -> Nothing
        CtxLAM       c _ _       -> Just c
        CtxLam       c _ _       -> Just c
        CtxAppLeft   c _ _       -> Just c
        CtxAppRight  c _ _       -> Just c
        CtxLetBody   c _ _       -> Just c
        CtxLetLLet   c _ _ _     -> Just c
        CtxLetLRec   c _ _ _ _ _ -> Just c
        CtxCaseScrut c _ _       -> Just c
        CtxCaseAlt   c _ _ _ _ _ -> Just c
        CtxCast      c _ _       -> Just c




