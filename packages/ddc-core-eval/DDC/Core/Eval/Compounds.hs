
-- | Utilities for constructing and destructing compound types and
--   expressions.
module DDC.Core.Eval.Compounds
        ( -- * Types
          tUnit
        , tInt
        , tList 

          -- * Expressions
        , isUnitX
        , takeHandleT
        , takeHandleX
        , takeLocX,     stripLocX
        , takeMutableX)
where
import DDC.Core.Eval.Name
import DDC.Type.Compounds
import DDC.Core.Exp


-- Type -----------------------------------------------------------------------
-- | Application of the Unit data type constructor.
tUnit :: Type Name
tUnit   = TCon (TyConBound (UPrim (NamePrimCon PrimTyConUnit) kData))


-- | Application of the Int data type constructor.
tInt :: Region Name -> Type Name
tInt r1 = TApp  (TCon (TyConBound (UPrim (NamePrimCon PrimTyConInt) 
                                  (kFun kRegion kData))))
                r1

-- | Application of the List data type constructor.
tList :: Region Name -> Type Name -> Type Name
tList tR tA
        = tApps (TCon  (TyConBound (UPrim (NamePrimCon PrimTyConList)
                                          (kRegion `kFun` kData `kFun` kData))))
                [tR, tA]


-- Exp ------------------------------------------------------------------------
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
--   We strip off 'forget' casts along the way
takeLocX :: Exp a Name -> Maybe Loc
takeLocX xx
 = case xx of
        XCast _ (CastForget _) x
         -> takeLocX x

        XCon _ (UPrim (NameLoc l) _)
                -> Just l
        _       -> Nothing


-- | Take a store location from an expression, reaching under any 'forget' casts.
stripLocX :: Exp a Name -> Maybe Loc
stripLocX xx
 = case xx of
        XCast _ (CastForget _) x
          -> stripLocX x

        XCon _ (UPrim (NameLoc l) _) 
          -> Just l

        _ -> Nothing


-- | Take a witness of mutability from an expression.
takeMutableX :: Exp a Name -> Maybe Rgn
takeMutableX xx
 = case xx of
        XWitness (WApp (WCon WiConMutable) (WType tR1))
                -> takeHandleT tR1
        _       -> Nothing



