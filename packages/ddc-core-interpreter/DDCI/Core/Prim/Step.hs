
module DDCI.Core.Prim.Step
        (primStep)
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDCI.Core.Prim.Store             (Store, SBind(..))
import qualified DDCI.Core.Prim.Store   as Store
-- import DDC.Core.Pretty
--import Debug.Trace


-- | Evaluation of primitive operators.
primStep
        :: Name
        -> [Exp () Name]
        -> Store
        -> Maybe (Store, Exp () Name)

primStep n xs store
-- = trace (show $ text "primStep: " <+> text (show n) <+> text (show xs))
 = primStep' n xs store

-- Alloction of integers.
primStep' (NameInt i) [xRegion, xUnit] store
        | XType tR@(TCon  (TyConBound (UPrim (NameRgn rgn) _)))  <- xRegion
        , XCon _   (UPrim (NamePrimCon PrimDaConUnit) _)         <- xUnit
        , Store.hasRgn store rgn
        , (store1, l)   <- Store.allocBind rgn (SInt i) store
        = Just  ( store1
                , XCon () (UPrim (NameLoc l) (tInt tR)))

primStep' (NamePrimOp PrimOpAdd) [xR1, xR2, xR3, x1, x2] store
        -- unpack the args
        | XType     (TCon (TyConBound (UPrim (NameRgn r1) _)))  <- xR1
        , XType     (TCon (TyConBound (UPrim (NameRgn r2) _)))  <- xR2
        , XType tR3@(TCon (TyConBound (UPrim (NameRgn r3) _)))  <- xR3
        , XCon _    (UPrim (NameLoc l1) _)                      <- x1
        , XCon _    (UPrim (NameLoc l2) _)                      <- x2

        -- get the regions and values of each location.
        , Just (r1', SInt i1)   <- Store.lookupRegionBind l1 store
        , Just (r2', SInt i2)   <- Store.lookupRegionBind l2 store
        
        -- the locations must be in the regions the args said they were in.
        , r1' == r1
        , r2' == r2
        
        -- the destination region must exist.
        , Store.hasRgn store r3

        -- do the actual computation
        , i3    <- i1 + i2
        
        -- write the result to a new location in the store.
        , (store1, l3)  <- Store.allocBind r3 (SInt i3) store

        = Just  ( store1
                , XCon () (UPrim (NameLoc l3) (tInt tR3)))

primStep' _ _ _
        = Nothing


