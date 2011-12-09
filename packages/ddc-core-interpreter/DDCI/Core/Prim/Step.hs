
module DDCI.Core.Prim.Step
        (primStep)
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDCI.Core.Prim.Store             (Store, SBind(..))
import qualified DDCI.Core.Prim.Store   as Store


-- | Evaluation of primitive operators.
primStep
        :: Name
        -> [Exp () Name]
        -> Store
        -> Maybe (Store, Exp () Name)

primStep (NameInt i) 
         [ XCon _ uR@(UPrim (NameRgn rgn) _)
         , XCon _ (UName (NamePrimCon PrimDaConUnit) _)]
         store
 = let  (store1, l)   = Store.allocBind rgn (SInt i) store
   in   Just  (store1
              , XCon () (UPrim   (NameLoc l) 
                                 (tInt (TCon (TyConBound uR)))))

