
-- | Single step evaluation of primitive operators and constructors.
---
--   This should implements the proper operational semantics of the core language,
--   so we're careful to check all premises of the evaluation rules are satisfied.
module DDC.Core.Eval.Prim
        ( stepPrimCon
        , stepPrimOp
        , primNewRegion
        , primDelRegion)
where
import DDC.Core.Eval.Compounds
import DDC.Core.Eval.Store
import DDC.Core.Eval.Name
import DDC.Core.Compounds
import DDC.Type.Compounds
import DDC.Core.Exp
import qualified DDC.Core.Eval.Store   as Store

-------------------------------------------------------------------------------
-- | Step a primitive constructor, which allocates an object in the store.
stepPrimCon
        :: DaCon Name           -- ^ Data constructor to evaluate.
        -> [Exp () Name]        -- ^ Arguments to constructor.
        -> Store                -- ^ Current store.
        -> Maybe ( Store        
                 , Exp () Name) -- ^ New store and result expression, 
                                --   if the operator steps, otherwise `Nothing`.


-- Redirect the unit constructor.
-- All unit values point to the same object in the store.
stepPrimCon DaConUnit [] store
        = Just  ( store
                , XCon () (DaConSolid (NameLoc locUnit) tUnit) )


-- Alloction of Ints.
stepPrimCon dc@(DaConAlgebraic (NameInt _) _) [xR, xUnit] store
        -- unpack the args
        | XType tR      <- xR
        , Just rgn      <- takeHandleT tR
        , isUnitOrLocX xUnit

        -- the store must contain the region we're going to allocate into.
        , Store.hasRgn store rgn

        -- add the binding to the store.
        , (store1, l)   <- Store.allocBind rgn 
                                (tInt tR) (SObj dc []) store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l) (tInt tR)))


-- Handle Pair specially until we have general data types.
stepPrimCon dc@(DaConAlgebraic (NamePrimCon PrimDaConPr) _) [xR, xA, xB, x1, x2] store
        -- unpack the args
        | XType tR      <- xR
        , Just rgn      <- takeHandleT tR
        , XType tA      <- xA
        , XType tB      <- xB
        , Just l1       <- takeLocX x1
        , Just l2       <- takeLocX x2

        -- the store must contain the region we're going to allocate into.
        , Store.hasRgn store rgn

        -- add the binding to the store
        , (store1, l)   <- Store.allocBind rgn
                                (tPair tR tA tB) (SObj dc [l1, l2]) store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l) (tPair tR tA tB)))


-- Handle Nil and Cons specially until we have general data types.
stepPrimCon dc@(DaConAlgebraic (NamePrimCon PrimDaConNil) _) [xR, xA, xUnit] store
        -- unpack the args
        | XType tR      <- xR
        , Just rgn      <- takeHandleT tR
        , XType tA      <- xA
        , isUnitOrLocX xUnit

        -- the store must contain the region we're going to allocate into.
        , Store.hasRgn store rgn

        -- add the binding to the store
        , (store1, l)   <- Store.allocBind rgn
                                (tList tR tA) (SObj dc []) store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l) (tList tR tA)))


stepPrimCon dc@(DaConAlgebraic (NamePrimCon PrimDaConCons) _) [xR, xA, xHead, xTail] store
        -- unpack the args
        | XType tR      <- xR
        , Just rgn      <- takeHandleT tR
        , XType tA      <- xA
        , Just lHead    <- takeLocX xHead
        , Just lTail    <- takeLocX xTail

        -- the store must contain the region we're going to allocate into.
        , Store.hasRgn store rgn

        -- add the binding to the store
        , (store1, l)   <- Store.allocBind rgn
                                (tList tR tA) (SObj dc [lHead, lTail]) store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l) (tList tR tA)))

stepPrimCon _ _ _
        = Nothing


-------------------------------------------------------------------------------
-- | Step a primitive operator.
stepPrimOp
        :: Name                 -- ^ Name of operator to evaluate.
        -> [Exp () Name]        -- ^ Arguments to operator.
        -> Store                -- ^ Current store.
        -> Maybe ( Store        
                 , Exp () Name) -- ^ New store and result expression, 
                                --   if the operator steps, otherwise `Nothing`.

-- Binary integer primop.
stepPrimOp (NamePrimOp op) [xR1, xR2, xR3, xL1, xL2] store
        -- unpack the args
        | Just fOp      <- lookup op 
                                [ (PrimOpAddInt, (+))
                                , (PrimOpSubInt, (-))
                                , (PrimOpMulInt, (*))
                                , (PrimOpDivInt, div) 
                                , (PrimOpEqInt,  (\x y -> if x == y then 1 else 0))]
        , Just r1       <- takeHandleX xR1
        , Just r2       <- takeHandleX xR2
        , XType tR3     <- xR3
        , Just r3       <- takeHandleX xR3        
        , Just l1       <- stripLocX xL1
        , Just l2       <- stripLocX xL2

        -- get the regions and values of each location
        , Just (r1', _, SObj (DaConAlgebraic (NameInt i1) tIntCon) [])  
                <- Store.lookupRegionTypeBind l1 store

        , Just (r2', _, SObj (DaConAlgebraic (NameInt i2) _)       [])
                <- Store.lookupRegionTypeBind l2 store
        
        -- the locations must be in the regions the args said they were in
        , r1' == r1
        , r2' == r2
        
        -- the destination region must exist
        , Store.hasRgn store r3

        -- do the actual computation
        , i3    <- i1 `fOp` i2
        
        -- write the result to a new location in the store
        , (store1, l3)  <- Store.allocBind r3 (tInt tR3) 
                                (SObj (DaConAlgebraic (NameInt i3) tIntCon) []) 
                                store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l3) (tInt tR3)))


-- Update integer primop.
stepPrimOp (NamePrimOp PrimOpUpdateInt) [xR1, xR2, xMutR1, xL1, xL2] store
        -- unpack the args
        | Just r1       <- takeHandleX  xR1
        , Just r2       <- takeHandleX  xR2
        , Just r1W      <- takeMutableX xMutR1
        , Just l1       <- stripLocX     xL1
        , Just l2       <- stripLocX     xL2      

        -- the witness must be for the destination region
        , r1W == r1

        -- get the regions and values of each location
        , Just (r1L, tX1, SObj dc1 [])       <- Store.lookupRegionTypeBind l1 store
        , DaConAlgebraic (NameInt _) tIntCon <- dc1

        , Just (r2L, _,   SObj dc2 [])  <- Store.lookupRegionTypeBind l2 store
        , Just (NameInt i2)             <- takeNameOfDaCon dc2

        -- the locations must be in the regions the args said they were in
        , r1L == r1
        , r2L == r2

        -- update the destination
        , dc'           <- DaConAlgebraic (NameInt i2) tIntCon
        , store1        <- Store.addBind l1 r1 tX1 (SObj dc' []) store
        = Just  ( store1
                , XCon () DaConUnit)

-- Unary integer operations
stepPrimOp (NamePrimOp op) [xR1, xR2, xL1] store
        -- unpack the args
        | Just fOp      <- lookup op
                                [ (PrimOpCopyInt, id)
                                , (PrimOpNegInt,  negate) ]
        , Just r1       <- takeHandleX  xR1
        , XType tR2     <- xR2
        , Just r2       <- takeHandleX  xR2
        , Just l1       <- stripLocX    xL1

        -- get the region and value of the int
        , Just (r1L, _, SObj dc1  [])           <- Store.lookupRegionTypeBind l1 store
        , DaConAlgebraic (NameInt i1) tIntCon   <- dc1

        -- the locations must be in the regions the args said they were in
        , r1L == r1

        -- the destination region must exist
        , Store.hasRgn store r2

	-- calculate
	, i2 <- fOp i1

        -- write the result to a new location in the store
        , dc'           <- DaConAlgebraic (NameInt i2) tIntCon
        , (store1, l2)  <- Store.allocBind r2 (tInt tR2) (SObj dc' []) store

        = Just  ( store1
                , XCon () (DaConSolid (NameLoc l2) (tInt tR2)))

stepPrimOp _ _ _
        = Nothing


-- Store ----------------------------------------------------------------------
-- | Like `Store.newRgn` but return the region handle wrapped in a `Bound`.
primNewRegion :: Store -> (Store, Bound Name)
primNewRegion store
 = let  (store', rgn)   = Store.newRgn store
        u               = UPrim (NameRgn rgn) kRegion
   in   (store', u)


-- | Like `Store.delRgn` but accept a region handle wrapped in a `Bound`.
primDelRegion :: Bound Name -> Store -> Maybe Store
primDelRegion uu store
 = case uu of
        UPrim (NameRgn rgn) _   -> Just $ Store.delRgn rgn store
        _                       -> Nothing

