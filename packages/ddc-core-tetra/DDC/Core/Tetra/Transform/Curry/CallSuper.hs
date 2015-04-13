
module DDC.Core.Tetra.Transform.Curry.CallSuper
        ( makeCallSuper
        , makeCallSuperSaturated
        , splitStdCall)
where
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Exp
import qualified DDC.Core.Call                  as Call


---------------------------------------------------------------------------------------------------
-- | Call a top-level supercombinator,
--   or foriegn function imported from Sea land.
makeCallSuper 
        :: Show a 
        => AnTEC a Name                         -- ^ Annotation to use.
        -> Name                                 -- ^ Name of super to call.
        -> Exp  (AnTEC a Name) Name             -- ^ Expression of super to call.

        -> [Type Name]                          -- ^ Parameter types of super
        -> Type Name                            -- ^ Return type of super.

        -> [Call.Elim (AnTEC a Name) Name]      -- ^ Value arguments for super.
        -> Bool                                 -- ^ Whether the result was run.
        -> Exp  (AnTEC a Name) Name

makeCallSuper aF _nF xF tsParamLam tResultSuper esArgValue bRun

 -- Fully saturated call to a super of foreign function. 
 -- We have arguments for each parameter, so can call it directly.
 | length esArgValue == length tsParamLam
 , xsArgValue   <- [x | Call.ElimValue _ x <- esArgValue]
 = makeRun aF bRun $ xApps aF xF xsArgValue

 -- Partially application of a super or foreign function.
 -- We need to build a closure, then attach any arguments we have.
 | length esArgValue <  length tsParamLam
 , xsArgValue   <- [x | Call.ElimValue _ x <- esArgValue]
 = let 
        -- Split types of the super parameters into the ones that can be
        -- satisfied by this application, and the remaining parameters that
        -- are still waiting for arguments.
        (tsParamSat, tsParamRemain)     
                = splitAt (length esArgValue) tsParamLam

        -- The type of the result after performing this application.
        -- If there are remaining, un-saturated parameters the result
        -- type will still be a function.
        Just tResultClo           = tFunOfList (tsParamRemain ++ [tResultSuper])

        tParamFirst : tsParamRest = tsParamLam
        Just tSuperResult         = tFunOfList (tsParamRest ++   [tResultSuper])

   in   makeRun aF bRun 
         $ xApps aF (xFunCurry aF tsParamSat tResultClo 
                           (xFunCReify aF tParamFirst tSuperResult xF))
                       xsArgValue

 -- TODO: handle over-applied super.
 --       do direct call, then do an application.
 --       the super must produce a closure, otherwise it won't be well typed.
 | otherwise
 , xsArgValue   <- [x | Call.ElimValue _ x <- esArgValue]
 = makeRun aF bRun $ xApps aF xF xsArgValue


----------------
-- | Fully saturated application.
--
--   When the eliminators at the call site exactly match the way the super
--   is constructed then we can call the super directoy.
--
makeCallSuperSaturated
        :: Show a
        => Exp a n              -- ^ Functional expression to call.
        -> [Call.Cons n]        -- ^ How the super is constructed.
        -> [Call.Elim a n]      -- ^ Arguments to eliminators at call site.
        -> Maybe (Exp a n)

makeCallSuperSaturated xF cs es
        | length cs == length es
        , and  $ zipWith Call.elimForCons es cs
        = Just $ foldl Call.applyElim xF es

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
-- | Check if these eliminators follow the standard super-call pattern.
--
--   The standard pattern is a list of type arguments, followed by some
--   value arguments, and optionally running the result suspension. 
--
--   @run f [T1] [T2] x1 x2@
--
--   If 'f' is a super, and this is a saturating call then the super header
--   will look like the following:
--
--   @f = (/\t1. /\t2. \v1. \v2. box. body)@
--
splitStdCall
        :: [Call.Elim a Name] 
        -> Maybe ([Call.Elim a Name], [Call.Elim a Name], Bool)

splitStdCall es
        | (esT, esMore)   <- span Call.isElimType   es
        , (esX, esMore2)  <- span Call.isElimValue  esMore
        , (esR, [])       <- span Call.isElimRun    esMore2
        ,  length esR <= 1
        = Just (esT, esX, length esR > 0)

        | otherwise
        = Nothing   
