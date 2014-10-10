module DDC.Core.Flow.Transform.Forward
        ( forwardProcesses )
where
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Module
import qualified DDC.Core.Simplifier                    as C

import qualified DDC.Core.Transform.Forward             as Forward
import qualified DDC.Core.Transform.TransformModX       as T

-- | Find all top-level Process bindings, and forward all non-series operators.
forwardProcesses :: Module () Name -> Module () Name
forwardProcesses mm
 = T.transformModX forwardX mm


forwardX :: Exp () Name -> Exp () Name
forwardX xx
 = C.result $ Forward.forwardX profile conf xx
 where
  conf = Forward.Config isFloatable False

  isFloatable lts
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
          | Just _ <- takeXLamFlags x
          -> Forward.FloatForceUsedOnce
        _ -> Forward.FloatAllow

