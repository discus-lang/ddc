
module DDC.Core.Tetra.Transform.Curry.Call
        (makeCall)
where
import DDC.Core.Tetra.Transform.Curry.CallSuper
import DDC.Core.Tetra.Transform.Curry.CallThunk
import DDC.Core.Tetra.Transform.Curry.Callable
import DDC.Core.Tetra.Transform.Curry.Error
import DDC.Core.Tetra.Prim
import DDC.Core.Exp
import Data.Map                                 (Map)
import qualified DDC.Core.Call                  as Call
import qualified Data.Map                       as Map


-- | Call a thing, depending on what it is.
--   Decide how to call the functional thing, depending on 
--   whether its a super, foreign imports, or thunk.
makeCall
        :: Map Name Callable    -- ^ Types and arities of functions in the environment.
        -> Name                 -- ^ Name of function to call. 
        -> Type Name            -- ^ Type of function to call.
        -> [Call.Elim () Name]  -- ^ Eliminators for function call.
        -> Either Error (Maybe (Exp () Name))

makeCall callables nFun tFun esArgs

 -- Call of a local or imported super.
 -- TODO: Check that the type in the table matches the one we were 
 --       given for the function.
 | Just (tF, csF)
    <- case Map.lookup nFun callables of
        Just (Callable _ tF csFun) -> Just (tF, csFun)
        _                          -> Nothing

 = case Call.dischargeConsWithElims csF esArgs of
        -- Saturating call.
        --  We have matching eliminators for all the constructors.
        ([], []) 
         -> makeCallSuperSaturated nFun csF esArgs

        -- Under application.
        --  The eliminators have all been used up,
        --  but the super that we're applying still has outer constructors.
        (_csRemain, [])
         -> makeCallSuperUnder     nFun tF csF esArgs

        -- Over application.
        --   The constructors have all been used up, 
        --   but we still have eliminators at the call site.
        ([], esOver)
         -> do  -- Split off enough eliminators to saturate the super.
                let nSat        = length csF
                let esSat       = take nSat esArgs

                -- Apply the super to all its arguments,
                -- which yields a thunk that wants more arguments.
                -- TODO: handle errors
                Just xApp       <- makeCallSuperSaturated nFun csF esSat

                -- Apply the resulting thunk to the remaining arguments.
                let Just tF'    = Call.dischargeTypeWithElims tF esSat

                makeCallThunk xApp tF' esOver

        -- Bad application.
        -- The eliminators we have do not match the constructors of the
        -- thing that we're applying. The program is mis-typed.
        (_, _)
         -> return $ Nothing    -- TODO: throw error

 -- Apply a thunk to some arguments.
 -- The functional part is a variable bound to a thunk object.
 | length esArgs > 0
 = makeCallThunk (XVar () (UName nFun)) tFun esArgs

 -- This was an existing thunk applied to no arguments,
 -- so we can just return it without doing anything.
 | otherwise
 = return $ Nothing

