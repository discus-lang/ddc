
module DDC.Core.Tetra.Transform.Curry.CallThunk
        (makeCallThunk)
where
import DDC.Core.Tetra.Transform.Curry.Error
import DDC.Core.Tetra.Prim
import DDC.Core.Exp.Annot
import qualified DDC.Core.Call                          as Call
import qualified DDC.Core.Tetra.Compounds               as C


-- | Apply a thunk to some more arguments.
--
--   The arguments must have be values, with type of kind `Data`.
--   If this is not true then `Nothing`.
--
makeCallThunk
        :: Exp () Name                  -- ^ Functional expression to apply.
        -> Type Name                    -- ^ Type of functional expression.
        -> [Call.Elim () Name]          -- ^ Eliminators for applicatoin.
        -> Either Error (Maybe (Exp () Name))

makeCallThunk xF tF esArgs

 -- Split the eliminators according to the standard call pattern.
 | Just ([], esValues, esRuns)  <- Call.splitStdCallElims esArgs
 = let  
        (tsParam, tResult)       = C.takeTFunArgResult tF

        -- Split the value parameters into ones applied to the thunk,
        -- and the ones that form part of its resulting type. 
        (tsParamArg, tsParamClo) = splitAt (length esValues) tsParam

        -- Build the type of the returned closure.
        --   Splitting the type like this assumes that the thunk 
        --   we're applying has a monomorphic type, which is true
        --   for thunked supers with standard calling convention as
        -- t  he types of these are all prenex.
        tResultClo      = C.tFunOfParamResult tsParamClo tResult

        xsArgs  = [ x | Call.ElimValue _ x <- esValues] 

   in  return 
         $ Just 
         $ makeRuns    () (length esRuns)
         $ C.xFunApply () tsParamArg tResultClo xF xsArgs

 | otherwise
 = return $ Nothing