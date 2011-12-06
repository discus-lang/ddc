
module DDCI.Core.Prim.Step
        (primStep)
where
import DDCI.Core.Prim.Base
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDCI.Core.Prim.Store             (Store, SBind(..))
import qualified DDCI.Core.Prim.Store   as Store


-- | Evaluation of primitive operators.
primStep
        :: Prim
        -> [Exp () Prim Name]
        -> Store
        -> Maybe (Store, Exp () Prim Name)

primStep (PInt i) [XPrim _ (PRgn rgn), XCon _ (UName tag@(Name "U") _)] store
 = let  (store1, loc)   = Store.allocBind rgn (SInt i) store
   in   Just (store1, XPrim () (PLoc loc))

