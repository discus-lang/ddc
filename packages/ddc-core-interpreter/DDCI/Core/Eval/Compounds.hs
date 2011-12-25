
module DDCI.Core.Eval.Compounds
        ( isUnitX
        , takeHandleT
        , takeHandleX
        , takeLocX
        , takeMutableX

          -- Store
        , primNewRegion
        , primDelRegion)
where
import DDCI.Core.Eval.Name
import DDC.Type.Compounds
import DDC.Core.Exp
import DDCI.Core.Eval.Store     as Store


-- | Check whether an expression is the unit constructor.
isUnitX :: Exp a Name -> Bool
isUnitX xx
 = case xx of
        XCon _   (UPrim (NamePrimCon PrimDaConUnit) _)  
                -> True
        _       -> False


-- | Take a region handle from a type.
takeHandleT :: Type Name -> Maybe Rgn
takeHandleT tt
 = case tt of
        TCon (TyConBound (UPrim (NameRgn r1) _))
                -> Just r1
        _       -> Nothing


-- | Take a region handle from an expression.
takeHandleX :: Exp a Name -> Maybe Rgn
takeHandleX xx
 = case xx of
        XType t -> takeHandleT t
        _       -> Nothing


-- | Take a store location from an expression.
takeLocX :: Exp a Name -> Maybe Loc
takeLocX xx
 = case xx of
        XCon _ (UPrim (NameLoc l) _)
                -> Just l
        _       -> Nothing


-- | Take a witness of mutability from an expression.
takeMutableX :: Exp a Name -> Maybe Rgn
takeMutableX xx
 = case xx of
        XWitness (WApp (WCon WiConMutable) (WType tR1))
                -> takeHandleT tR1
        _       -> Nothing


-- Store ----------------------------------------------------------------------
primNewRegion :: Store -> (Store, Bound Name)
primNewRegion store
 = let  (store', rgn)   = Store.newRgn store
        u               = UPrim (NameRgn rgn) kRegion
   in   (store', u)


primDelRegion :: Bound Name -> Store -> Maybe Store
primDelRegion uu store
 = case uu of
        UPrim (NameRgn rgn) _   -> Just $ Store.delRgn rgn store
        _                       -> Nothing


