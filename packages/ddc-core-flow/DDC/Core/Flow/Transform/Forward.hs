module DDC.Core.Flow.Transform.Forward
        ( forwardProcesses )
where
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Module
import qualified DDC.Core.Simplifier                    as C

import qualified DDC.Core.Transform.Forward             as Forward
import qualified DDC.Core.Transform.TransformModX       as T

-- | Find all top-level Process bindings, and forward all non-series operators.
-- This is a bit of a hack, because lower doesn't accept any non-series bindings.
forwardProcesses :: Module () Name -> Module () Name
forwardProcesses mm
 = T.transformModLet forwardBind mm


-- | Forward a single process binding
forwardBind :: Bind Name -> Exp () Name -> Exp () Name
forwardBind b xx

 -- If the result type of a top-level binding is a Process,
 -- we must prepare it for the lowering transform.
 -- Forward everything we can, while leaving series operators at the top.
 | isProcessType $ snd $ takeTFunAllArgResult $ typeOfBind b
 = C.result $ Forward.forwardX profile conf_process xx

 -- Otherwise do minimal forwarding, except for pushing any rate-valued functions
 -- into their runKernel#.
 | otherwise
 = C.result $ Forward.forwardX profile conf_nonproc xx
 where
  conf_process = Forward.Config isFloatable_process False
  conf_nonproc = Forward.Config isFloatable_nonproc False

  -- Deny forwarding of flow primitives.
  -- Force anything else that's used only once.
  --
  -- For lower to work, we need to forward everything except primitives,
  -- but that duplicates work. Lower should probably be changed.
  isFloatable_process lts
     = case lts of
        LLet (BName _ _) x
          | Just (n,_) <- takeXPrimApps x
          -> case n of
             NameOpConcrete _   -> Forward.FloatDeny
             NameOpControl  _   -> Forward.FloatDeny
             NameOpSeries   _   -> Forward.FloatDeny
             NameOpStore    _   -> Forward.FloatDeny
             NameOpVector   _   -> Forward.FloatDeny

             _                  -> Forward.FloatForceUsedOnce
        _ -> Forward.FloatForceUsedOnce


  -- Forward any Process functions - they will have Rate foralls inside them.
  isFloatable_nonproc lts
     = case lts of
        LLet _ x
          | Just (lams,_) <- takeXLamFlags x
          , any (\(_,bo) -> typeOfBind bo == kRate) lams
          -> Forward.FloatForce
        _ -> Forward.FloatAllow

