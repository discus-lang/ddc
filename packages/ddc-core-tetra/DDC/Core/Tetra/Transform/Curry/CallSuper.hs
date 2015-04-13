
module DDC.Core.Tetra.Transform.Curry.CallSuper
        ( makeCallSuper
        , makeCallSuperSaturated
        , makeCallSuperUnder
        , splitStdCall)
where
import DDC.Core.Tetra.Transform.Curry.Interface
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Exp
import Data.Maybe
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


---------------------------------------------------------------------------------------------------
-- | Fully saturated application.
--
--   When the eliminators at the call site exactly match the way the super
--   is constructed then we can call the super directly. In the generated
--   object code we do a standard function call.
--
makeCallSuperSaturated
        :: a                    -- ^ Annotation from var node that mentioned super.
        -> n                    -- ^ Name of super to call.
        -> [Call.Cons n]        -- ^ How the super is constructed.
        -> [Call.Elim a n]      -- ^ Eliminators at call site.
        -> Maybe (Exp a n)

makeCallSuperSaturated aF nF cs es
        | length es == length cs
        , and  $ zipWith Call.elimForCons es cs
        = Just $ foldl Call.applyElim (XVar aF (UName nF)) es

        | otherwise     = Nothing


---------------------------------------------------------------------------------------------------
-- | Under saturated application.
--
--   When we don't have enough eliminators to match all the constructors in
--   the function header then the application is under-saturated.
--
--   We build a PAP object to store the arguments we have at the moment,
--   and the runtime will wait until we have the full set until calling
--   the underlying super.
--
--   This only works for supers in the standard form,
--    eg /\(a1 : k1). .. /\(a2 : k1). \(x1 : t1). .. \(x2 : t2). box
--
--   At the call site we must provide type arguments to satify
--   all the type parameters, but don't need to supply all the value
--   arguments, or to run the box. We restrict the call pattern this
--   way to make the runtime easier to write, and so that we can implement
--   perform PAP construction and elimination using primitives with
--   straightforward types. The code program needs to be normalised to use
--   standard calls before undergoing the curry transform.
--
makeCallSuperUnder
        :: a                    -- ^ Annotation from var node that mentioned super.
        -> Name                 -- ^ Name of super to call.
        -> Type Name            -- ^ Type of super.
        -> [Call.Cons Name]     -- ^ How the super is constructed.
        -> [Call.Elim a Name]   -- ^ Eliminators at call site.
        -> Maybe (Exp a Name)

makeCallSuperUnder aF nF tF cs esArgs
        -- We don't have enough eliminators for all the constructors.
        | length esArgs         <  length cs
        , iArity                <- length cs

        -- The call must be in standard form.
        , Just (esArgsType, esArgsValue, bRun) <- splitStdCall esArgs
        , xsArgType             <- [XType at t  | Call.ElimType  _ at t  <- esArgsType]
        , xsArgValue            <- [x           | Call.ElimValue _ x     <- esArgsValue]

        -- Split the quantifiers, parameter type, and body type
        -- from the type of the super.
        , (bsForall, tBody)     <- fromMaybe ([], tF) $ takeTForalls tF
        , (tsParam,  tResult)   <- takeTFunArgResult tBody
        
        -- There must be types to satisfy all of the type paramters of the super.
        , length bsForall == length esArgsType

        -- Split the value parameters into ones accepted by the super,
        -- and ones that are accepted by the returned closures.
        , (tsParamLam, tsParamClo) <- splitAt iArity tsParam
        
        -- Build the type of the returned value.
        , Just tResult'         <- tFunOfList (tsParamClo ++ [tResult])
        
        -- Instantiate all the type parameters.
        , xFunAPP               <- (xApps aF (XVar aF (UName nF)) xsArgType)

        -- Split types of the super parameters into the ones that can be
        -- satisfied by this application, and the remaining parameters that
        -- are still waiting for arguments.
        , (tsParamSat, tsParamRemain)     
                                <- splitAt (length xsArgValue) tsParamLam

        -- The type of the result after performing this application.
        -- If there are remaining, un-saturated parameters the result
        -- type will still be a function.
        , Just tResultClo       <- tFunOfList (tsParamRemain ++ [tResult'])

        , tParamFirst : tsParamRest <- tsParamLam
        , Just tSuperResult     <- tFunOfList (tsParamRest ++   [tResult'])

        =  Just 
        $  makeRun aF bRun 
        $  xApps aF (xFunCurry aF tsParamSat tResultClo 
                           (xFunCReify aF tParamFirst tSuperResult xFunAPP))
                       xsArgValue

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
