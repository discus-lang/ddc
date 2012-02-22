
-- | Utilities for constructing and destructing compound types and
--   expressions.
module DDC.Core.Eval.Compounds
        ( -- * Types
          tUnit
        , tInt
        , tPair
        , tList 

          -- * Witnesses
        , wGlobal
        , wConst,    wMutable
        , wLazy,     wManifest
        , wcGlobal
        , wcConst,   wcMutable
        , wcLazy,    wcManifest
        , isCapConW

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
-- | Application of the Unit type constructor.
tUnit :: Type Name
tUnit   = TCon (TyConBound (UPrim (NamePrimCon PrimTyConUnit) kData))


-- | Application of the Int type constructor.
tInt :: Region Name -> Type Name
tInt r1 = TApp  (TCon (TyConBound (UPrim (NamePrimCon PrimTyConInt) 
                                  (kFun kRegion kData))))
                r1

-- | Application of the Pair type constructor.
tPair :: Region Name -> Type Name -> Type Name -> Type Name
tPair tR tA tB
        = tApps (TCon  (TyConBound (UPrim (NamePrimCon PrimTyConPair)
                                          (kFuns [kRegion, kData, kData] kData))))
                [tR, tA, tB]


-- | Application of the List type constructor.
tList :: Region Name -> Type Name -> Type Name
tList tR tA
        = tApps (TCon  (TyConBound (UPrim (NamePrimCon PrimTyConList)
                                          (kRegion `kFun` kData `kFun` kData))))
                [tR, tA]


-- Witness --------------------------------------------------------------------
wGlobal     :: Region Name -> Witness Name
wGlobal r   = WApp (WCon wcGlobal)   (WType r)

wConst      :: Region Name -> Witness Name
wConst r    = WApp (WCon wcConst)    (WType r)

wMutable    :: Region Name -> Witness Name
wMutable r  = WApp (WCon wcMutable)  (WType r)

wLazy       :: Region Name -> Witness Name
wLazy r     = WApp (WCon wcLazy)     (WType r)

wManifest   :: Region Name -> Witness Name
wManifest r = WApp (WCon wcManifest) (WType r)


-- Just the Constructors
wcGlobal   :: WiCon Name
wcGlobal   = WiConBound 
           $ UPrim (NameCap CapGlobal)   (tForall kRegion $ \r -> tGlobal r)

wcConst    :: WiCon Name
wcConst    = WiConBound
           $ UPrim (NameCap CapConst)    (tForall kRegion $ \r -> tConst r)

wcMutable  :: WiCon Name
wcMutable  = WiConBound
           $ UPrim (NameCap CapMutable)  (tForall kRegion $ \r -> tMutable r)

wcLazy     :: WiCon Name
wcLazy     = WiConBound
           $ UPrim (NameCap CapLazy)     (tForall kRegion $ \r -> tLazy r)

wcManifest :: WiCon Name
wcManifest = WiConBound
           $ UPrim (NameCap CapManifest) (tForall kRegion $ \r -> tManifest r)
      

-- | Check whether a witness is a capability constructor.
isCapConW :: Witness Name -> Bool
isCapConW ww
 = case ww of
        WCon WiConBound{}     -> True
        _                     -> False



-- Exp ------------------------------------------------------------------------
-- | Check whether an expression is the unit constructor.
isUnitX :: Exp a Name -> Bool
isUnitX xx
 = case xx of
        XCon _   (UPrim (NamePrimCon PrimDaConUnit) _)  -> True
        _                                               -> False


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
        XWitness (WApp (WCon wc) (WType tR1))
         | WiConBound (UPrim (NameCap CapMutable) _) <- wc
                -> takeHandleT tR1
        _       -> Nothing



