module DDC.Core.Flow.Transform.Forward
        ( forwardProcesses )
where
import DDC.Core.Flow.Transform.Slurp
import DDC.Core.Flow.Compounds (tProcess)
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
-- import DDC.Core.Flow.Exp
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Module
import qualified DDC.Core.Simplifier                    as C

import DDC.Core.Transform.Deannotate

import qualified DDC.Core.Transform.Forward             as Forward

-- | Find all top-level Process bindings, and forward all non-series operators.
forwardProcesses :: Module () Name -> Module () Name
forwardProcesses mm
 = let body      = moduleBody mm

       (lets,xx) = splitXLets body
       lets'     = map forwardLets lets

       body'     = xLets () lets' xx

   in  mm { moduleBody = body' }

-- | Slurp stream processes from the top-level let expressions.
forwardLets :: Lets () Name -> Lets () Name
forwardLets (LRec binds)
 = LRec [forwardBind b x | (b, x) <- binds]

forwardLets (LLet b x)
 = uncurry LLet (forwardBind b x)

forwardLets l
 = l


forwardBind 
        :: Bind Name            -- ^ Binder for the whole process.
        -> Exp () Name          -- ^ Expression of body.
        -> (Bind Name, Exp () Name)

forwardBind b@(BName _ t) xx
 -- We assume that all type params come before the value params.
 | (snd $ takeTFunAllArgResult t) == tProcess
 = (b, forwardX xx)

forwardBind b             xx
 = (b,          xx)
        

forwardX :: Exp () Name -> Exp () Name
forwardX xx
 = C.result $ Forward.forwardX profile conf xx
 where
  conf = Forward.Config isFloatable False

  isFloatable lts
     = case lts of
        LLet (BName _ _) x
          |  isSeriesOperator (deannotate (const Nothing) x)
          -> Forward.FloatDeny
        _ -> Forward.FloatForce

