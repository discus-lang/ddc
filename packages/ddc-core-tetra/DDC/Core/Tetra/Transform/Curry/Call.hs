
module DDC.Core.Tetra.Transform.Curry.Call
        (makeCall)
where
import DDC.Core.Tetra.Transform.Curry.CallSuper
import DDC.Core.Tetra.Transform.Curry.CallThunk
import DDC.Core.Tetra.Transform.Curry.Interface

import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Exp
import Data.Maybe
import qualified DDC.Core.Call                  as Call
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- | Call a thing, depending on what it is.
--   Decide how to call the functional thing, depending on 
--   whether its a super, foreign imports, or thunk.
makeCall
        :: Show a
        => Exp (AnTEC a Name) Name
        -> AnTEC a Name         -- ^ Annotation that contains the type of the function
                                --   that we're applying.
        -> FunMap               -- ^ Types and arities of functions in the environment.
        -> Name                 -- ^ Name of function to call.
        -> [Call.Elim (AnTEC a Name) Name]    
                                -- ^ Arguments to eliminators.
        ->  Exp (AnTEC a Name) Name

makeCall xx aF funMap nF esArgs

        ---------------------------------------------------
        -- Direct call of a top-level super, 
        --  either defined in the local module, 
        --  an external module,
        --  or impoted via the foreign function interface.
        --
        | Just (tF, iArity) 
            <- case Map.lookup nF funMap of
                Just (FunLocalSuper  _ tF iArity)       -> Just (tF, iArity)
                Just (FunExternSuper _ tF iArity)       -> Just (tF, iArity)
                Just (FunForeignSea  _ tF iArity)       -> Just (tF, iArity)
                _                                       -> Nothing
        
        -- split the quantifiers from the type of the super.
        , (bsForall, tBody)             <- fromMaybe ([], tF) $ takeTForalls tF
        
        -- split the body type into value parameters and result.
        , (tsParam, tResult)            <- takeTFunArgResult tBody
        
        -- split the value parameters into ones accepted by lambdas and ones that 
        -- are accepted by the returned closure.
        , (tsParamLam, tsParamClo)      <- splitAt iArity tsParam
        
        -- build the type of the returned value.
        , Just tResult'                 <- tFunOfList (tsParamClo ++ [tResult])
        
        -- split the arguments into the type arguments that satisfy the quantifiers,  
        -- then the value arguments.
        , Just (esTypes, esValues, bRun) <- splitStdCall esArgs
        , xsArgTypes    <- [XType a t   | Call.ElimType  a t <- esTypes]
        , esArgValues   <- filter Call.isElimValue esValues

        -- there must be types to satisfy all of the quantifiers
        , length bsForall == length xsArgTypes
 
        = makeCallSuper aF nF
                (xApps aF (XVar aF (UName nF)) xsArgTypes)
                tsParamLam 
                tResult' 
                esArgValues
                bRun

        ---------------------------------------------------
        -- | Apply a thunk to its arguments.
        --   The functional part is a variable bound to a thunk object.
        | length esArgs > 0
        , Just (esTypes, esValues, bRun) <- splitStdCall esArgs
        , xsArgTypes    <- [XType a  t  | Call.ElimType  a t <- esTypes]
        , xsArgValues   <- [x           | Call.ElimValue _ x <- esValues]
        = makeCallThunk aF nF (xsArgTypes ++ xsArgValues) bRun

        ---------------------------------------------------
        -- | This was an existing thunk applied to no arguments,
        --   so we can just return it without doing anything.
        | otherwise
        = xx
