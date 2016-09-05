
module DDC.Core.Transform.Lambdas.Base
        ( S (..), evalState
        , newVar
        , newVarExtend
        , isLiftyContext)
where
import DDC.Core.Exp.Annot.Ctx
import DDC.Core.Exp.Annot
import DDC.Base.Name
import qualified Control.Monad.State.Strict     as S


---------------------------------------------------------------------------------------------------
-- | State holding a variable name prefix and counter to 
--   create fresh variable names.
type S  = S.State (String, Int)


-- | Evaluate a desguaring computation,
--   using the given prefix for freshly introduced variables.
evalState :: String -> S a -> a
evalState n c
 = S.evalState c (n, 0) 


-- | Allocate a new named variable, yielding its associated bind and bound.
newVar 
        :: CompoundName n
        => String       -- ^ Informational name to add.
        -> Type n       -- ^ Type of the new binder.
        -> S (Bind n, Bound n)

newVar prefix t
 = do   (n, i)   <- S.get
        let name' = newVarName (n ++ "$" ++ prefix ++ "$" ++ show i)
        S.put (n, i + 1)
        return  (BName name' t, UName name')


-- | Allocate a new named variable, yielding its associated bind and bound.
newVarExtend
        :: CompoundName n
        => n            -- ^ Base name.
        -> String       -- ^ Informational name to ad.
        -> Type n       -- ^ Type of the new binder.
        -> S (Bind n, Bound n)

newVarExtend name prefix t
 = do   (n, i)   <- S.get
        let name' = extendName name (n ++ "$" ++ prefix ++ "$" ++ show i)
        S.put (n, i + 1)
        return  (BName name' t, UName name')


---------------------------------------------------------------------------------------------------
-- | Check if this is a context that we should lift lambda abstractions out of.
isLiftyContext :: Ctx a n -> Bool
isLiftyContext ctx
 = case ctx of
        -- Don't lift out of the top-level context.
        -- There's nowhere else to lift to.
        CtxTop{}        -> False
        CtxLetLLet{}    -> not $ isTopLetCtx ctx
        CtxLetLRec{}    -> not $ isTopLetCtx ctx

        -- Don't lift if we're inside more lambdas.
        --  We want to lift the whole binding group together.
        CtxLAM{}        -> False
        CtxLam{}        -> False
   
        -- We can't do code generation for abstractions in these contexts,
        -- so they need to be lifted.
        CtxAppLeft{}    -> True
        CtxAppRight{}   -> True
        CtxLetBody{}    -> True
        CtxCaseScrut{}  -> True
        CtxCaseAlt{}    -> True
        CtxCastBody{}   -> True

