{-# OPTIONS_HADDOCK hide #-}
-- | Simple predicates on core expressions.
module DDC.Core.Exp.Generic.Predicates
        ( module DDC.Type.Exp.Simple.Predicates

          -- * Atoms
        , isXVar,  isXCon
        , isAtomX, isAtomR, isAtomW

          -- * Abstractions
        , isXAbs, isXLAM, isXLam

          -- * Applications
        , isXApp

          -- * Let bindings
        , isXLet

          -- * Patterns
        , isPDefault)
where
import DDC.Core.Exp.Generic.Exp
import DDC.Type.Exp.Simple.Predicates


-- Atoms ----------------------------------------------------------------------
-- | Check whether an expression is a variable.
isXVar :: GExp l -> Bool
isXVar xx
 = case xx of
        XVar{}  -> True
        _       -> False


-- | Check whether an expression is a constructor.
isXCon :: GExp l -> Bool
isXCon xx
 = case xx of
        XCon{}  -> True
        _       -> False


-- | Check whether an expression is an atomic value,
--   eg an `XVar`, `XCon`, or `XPrim`.
isAtomX :: GExp l -> Bool
isAtomX xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XPrim{}         -> True
        _               -> False


-- | Check whether an argument is an atomic value,
isAtomR :: GArg l -> Bool
isAtomR aa
 = case aa of
        RWitness w      -> isAtomW w
        RExp x          -> isAtomX x
        RType t         -> isAtomT t


-- | Check whether a witness is a `WVar` or `WCon`.
isAtomW :: GWitness l -> Bool
isAtomW ww
 = case ww of
        WVar{}          -> True
        WCon{}          -> True
        _               -> False


-- Abstractions ---------------------------------------------------------------
-- | Check whether an expression is an abstraction.
isXAbs :: GExp l -> Bool
isXAbs xx
 = case xx of
        XAbs{}  -> True
        _       -> False


-- | Check whether an expression is a spec abstraction (level-1).
isXLAM :: GExp l -> Bool
isXLAM xx
 = case xx of
        XLAM{}  -> True
        _       -> False


-- | Check whether an expression is a value or witness abstraction (level-0).
isXLam :: GExp l -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False


-- Applications ---------------------------------------------------------------
-- | Check whether an expression is an `XApp`.
isXApp :: GExp l -> Bool
isXApp xx
 = case xx of
        XApp{}  -> True
        _       -> False


-- Let Bindings ---------------------------------------------------------------
-- | Check whether an expression is a `XLet`.
isXLet :: GExp l -> Bool
isXLet xx
 = case xx of
        XLet{}  -> True
        _       -> False


-- Patterns -------------------------------------------------------------------
-- | Check whether an alternative is a `PDefault`.
isPDefault :: GPat l -> Bool
isPDefault pp
 = case pp of
        PDefault -> True
        _        -> False

