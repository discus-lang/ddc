
module DDC.Core.Tetra.Transform.Curry.CallThunk
        (makeCallThunk)
where
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import qualified DDC.Core.Predicates                    as C
import qualified DDC.Core.Tetra.Compounds               as C
import DDC.Core.Exp


-- | Apply a thunk to some more arguments.
--
--   The arguments must have be values, with type of kind `Data`.
--   If this is not true then `Nothing`.
--
makeCallThunk
        :: Show a
        => AnTEC a Name                 -- ^ Annotation from functional part of application.
        -> Name                         -- ^ Name of thunk.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to thunk.
        -> Int                          -- ^ How many times to run the result.
        -> Maybe (Exp (AnTEC a Name) Name)

makeCallThunk aF nF xsArgs nRuns

 -- This only works for value arguments.
 | all (\x -> (not . C.isXType)    x 
           && (not . C.isXWitness) x) xsArgs
 = let  
        (tsParam, tResult)       = C.takeTFunArgResult $ annotType aF

        -- Split the value paramters into ones applied to the thunk,
        -- and the ones that form part of its resulting type. 
        (tsParamArg, tsParamClo) = splitAt (length xsArgs) tsParam

        -- Build the type of the returned closure.
        Just tResultClo          = C.tFunOfList (tsParamClo ++ [tResult])

   in   Just 
         $ makeRuns aF nRuns
         $ C.xFunApply aF tsParamArg tResultClo (XVar aF (UName nF)) xsArgs

 | otherwise
 = Nothing
