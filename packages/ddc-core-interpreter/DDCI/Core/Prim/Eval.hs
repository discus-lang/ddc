
module DDCI.Core.Prim.Eval
        (primEval)
where
import DDCI.Core.Prim.Base
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDCI.Core.Prim.Store             (Store, SBind(..))
import qualified DDCI.Core.Prim.Store   as Store

-- | Evaluation of primitive operators.
primEval 
        :: Prim
        -> [Exp () Prim Name]
        -> Store
        -> Maybe (Store, Exp () Prim Name)

primEval (PInt i) [XPrim _ (PRgn rgn), XCon _ (UName tag@(Name "U") _)] store
 = let  (store1, loc)   = Store.allocBind rgn (SInt i) store
   in   Just (store1, XPrim () (PLoc loc))

