
module DDC.Core.Tetra.Transform.Curry.Call
        (makeCall)
where
import DDC.Core.Tetra.Transform.Curry.CallSuper
import DDC.Core.Tetra.Transform.Curry.CallThunk
import DDC.Core.Tetra.Transform.Curry.Interface

import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Exp
import qualified DDC.Core.Call                  as Call
import qualified Data.Map                       as Map
import qualified Text.Show.Pretty               as Text

---------------------------------------------------------------------------------------------------
-- | Call a thing, depending on what it is.
--   Decide how to call the functional thing, depending on 
--   whether its a super, foreign imports, or thunk.
makeCall
        :: Show a
        => FunMap                       -- ^ Types and arities of functions in the environment.
        -> Exp (AnTEC a Name) Name      -- ^ Overall application expression, for error reporting.
        -> Name                         -- ^ Name of function to call.
        -> AnTEC a Name                 -- ^ Annotation that contains the type of the function
                                        --   that we're applying.
        -> [Call.Elim (AnTEC a Name) Name]    
                                        -- ^ Arguments to eliminators.
        ->  Exp (AnTEC a Name) Name

makeCall funMap xx nF aF esArgs

        ---------------------------------------------------
        -- Call of a local or imported super.
        | Just (tF, csF)
            <- case Map.lookup nF funMap of
                Just (FunLocalSuper  _ tF _ csFun)        -> Just (tF, csFun)
                Just (FunExternSuper _ tF _ (Just csFun)) -> Just (tF, csFun)
                Just (FunForeignSea  _ tF _ csFun)        -> Just (tF, csFun)
                _                                         -> Nothing

        = case Call.dischargeElimsWithConss csF esArgs of
                -- Saturating call.
                --  We have matching eliminators for all the constructors.
                ([], []) 
                 |  Just xResult <- makeCallSuperSaturated aF nF csF esArgs
                 -> xResult

                -- Under application.
                --  The eliminators have all been used up,
                --  but the super that we're applying still has outer constructors.
                (_csRemain, [])
                 |  Just xResult <- makeCallSuperUnder     aF nF tF csF esArgs
                 -> xResult

                -- Over application.
                --   The constructors have all been used up, 
                --   but we still have eliminators at the call site.
                ([], _esRemain)
                 -> error $  "makeCall: over application"
                          ++ Text.ppShow (nF, esArgs, csF)

                -- Bad application.
                -- The eliminators we have do not match the constructors of the thing
                -- that we're applying. The program is mis-typed.
                (_, _)
                 -> error "makeCall: bad application"


        ---------------------------------------------------
        -- Apply a thunk to some arguments.
        -- The functional part is a variable bound to a thunk object.
        | length esArgs > 0
        , Just (esTypes, esValues, bRun) <- splitStdCallElim esArgs
        , xsArgTypes    <- [XType a  t  | Call.ElimType  a _ t <- esTypes]
        , xsArgValues   <- [x           | Call.ElimValue _ x <- esValues]
        , Just xResult  <- makeCallThunk aF nF (xsArgTypes ++ xsArgValues) bRun
        = xResult


        ---------------------------------------------------
        -- This was an existing thunk applied to no arguments,
        -- so we can just return it without doing anything.
        | otherwise
        = xx

