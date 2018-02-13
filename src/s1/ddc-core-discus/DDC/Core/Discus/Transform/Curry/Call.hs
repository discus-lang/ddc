
module DDC.Core.Discus.Transform.Curry.Call
        (makeCall)
where
import DDC.Core.Discus.Transform.Curry.CallSuper
import DDC.Core.Discus.Transform.Curry.CallThunk
import DDC.Core.Discus.Transform.Curry.Callable
import DDC.Core.Discus.Transform.Curry.Error
import DDC.Core.Discus.Prim
import DDC.Core.Exp
import DDC.Type.Exp.Simple.Equiv
import Control.Monad
import Data.Map                                 (Map)
import DDC.Core.Env.EnvT                        (EnvT)
import qualified DDC.Core.Call                  as Call
import qualified Data.Map                       as Map


-- | Call a thing, depending on what it is.
--   Decide how to call the functional thing, depending on
--   whether its a super, foreign imports, or thunk.
makeCall :: EnvT Name
         -> Map  Name Callable   -- ^ Types and arities of functions in the environment.
         -> Name                 -- ^ Name of function to call.
         -> Type Name            -- ^ Type of function to call.
         -> [Call.Elim () Name]  -- ^ Eliminators for function call.
         -> Either Error (Maybe (Exp () Name))

makeCall envt callables nFun tFun esArgs

 -- Call of a local or imported super.
 | Just (tFunTable, csF)
    <- case Map.lookup nFun callables of
        Just (Callable _ tFunTable csFun) -> Just (tFunTable, csFun)
        _                                 -> Nothing
 = do
        -- Internal sanity check: the type annotation on the function
        -- to call should match the type we have for it in the callables
        -- table. If not then we're bugged.
        when (not $ equivT envt tFun tFunTable)
         $ Left $ ErrorSuperTypeMismatch nFun tFun tFunTable

        case Call.dischargeConsWithElims csF esArgs of
         -- Saturating call.
         --  We have matching eliminators for all the constructors.
         ([], [])
          -> fmap Just $ makeCallSuperSaturated nFun csF esArgs

         -- Under application.
         --  The eliminators have all been used up,
         --  but the super that we're applying still has outer constructors.
         --  We need to build a PAP object to store the eliminators we have,
         --  rather than calling the super right now.
         (_csRemain, [])
          -> makeCallSuperUnder nFun tFun csF esArgs

         -- Over application.
         --   The constructors have all been used up,
         --   but we still have eliminators at the call site.
         ([], esOver)
          -> do -- Split off enough eliminators to saturate the super.
                let nSat  = length csF
                let esSat = take nSat esArgs

                -- Apply the super to all its arguments,
                -- which yields a thunk that wants more arguments.
                xApp     <- makeCallSuperSaturated nFun csF esSat

                -- Work out the type of the returned thunk.
                --  If this fails then the expression was mis-typed,
                --  or the arity information we had was wrong.
                tFun'    <- case Call.dischargeTypeWithElims tFun esSat of
                                Just tFun' -> return tFun'
                                Nothing    -> Left $ ErrorSuperCallPatternMismatch
                                                      nFun (Just tFun) Nothing esSat

                -- Apply the resulting thunk to the remaining arguments.
                makeCallThunk xApp tFun' esOver

         -- Bad application.
         -- The eliminators we have do not match the constructors of the
         -- thing that we're applying. The program is mis-typed.
         (_, _)
          -> Left $ ErrorSuperCallPatternMismatch
                        nFun (Just tFun) Nothing esArgs

 -- Apply a thunk to some arguments.
 -- The functional part is a variable bound to a thunk object.
 | length esArgs > 0
 = makeCallThunk (XVar () (UName nFun)) tFun esArgs

 -- This was an existing thunk applied to no arguments,
 -- so we can just return it without doing anything.
 | otherwise
 = return $ Nothing

