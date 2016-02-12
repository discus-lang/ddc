
module DDC.Core.Tetra.Transform.Curry.CallThunk
        (makeCallThunk)
where
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import qualified DDC.Core.Call                          as Call
import qualified DDC.Core.Tetra.Compounds               as C
import DDC.Core.Exp


-- | Apply a thunk to some more arguments.
--
--   The arguments must have be values, with type of kind `Data`.
--   If this is not true then `Nothing`.
--
makeCallThunk
        :: Show a
        => AnTEC a Name                         -- ^ Annotation for result expression.
        -> Exp (AnTEC a Name) Name              -- ^ Functional expression to apply.
        -> Type Name                            -- ^ Type of functional expression.
        -> [Call.Elim (AnTEC a Name) Name]      -- ^ Eliminators for applicatoin.
        -> Maybe (Exp (AnTEC a Name) Name)

makeCallThunk aF xF tF esArgs

 = let  Just ([], esValues, esRuns)
                = Call.splitStdCallElims esArgs

        (tsParam, tResult)       = C.takeTFunArgResult tF

        -- Split the value parameters into ones applied to the thunk,
        -- and the ones that form part of its resulting type. 
        (tsParamArg, tsParamClo) = splitAt (length esValues) tsParam

        -- Build the type of the returned closure.
        -- TODO: use dischargs fns instead, this won't work for polytypes.
        Just tResultClo          = C.tFunOfList (tsParamClo ++ [tResult])

        xsArgs  = [ x | Call.ElimValue _ x <- esValues] 

   in   Just 
         $ makeRuns    aF (length esRuns)
         $ C.xFunApply aF tsParamArg tResultClo xF xsArgs

