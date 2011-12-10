
module DDCI.Core.Prim.Step
        (primStep)
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDC.Core.Pretty
import DDCI.Core.Prim.Store             (Store, SBind(..))
import qualified DDCI.Core.Prim.Store   as Store
import Debug.Trace


-- | Evaluation of primitive operators.
primStep
        :: Name
        -> [Exp () Name]
        -> Store
        -> Maybe (Store, Exp () Name)

primStep n xs store
 = trace (show $ text "primStep: " <+> text (show n) <+> text (show xs))
 $ primStep' n xs store

primStep' (NameInt i) [xRegion, xUnit] store
        | XType tR@(TCon  (TyConBound (UPrim (NameRgn rgn) _)))  <- xRegion
        , XCon _   (UPrim (NamePrimCon PrimDaConUnit) _)         <- xUnit
        , Store.hasRgn store rgn
        , (store1, l)   <- Store.allocBind rgn (SInt i) store
        = Just  ( store1
                , XCon () (UPrim (NameLoc l) (tInt tR)))

primStep' _ _ _
        = Nothing
