
-- | Simple predicates on Source Tetra things.
module DDC.Source.Tetra.Predicates
        ( module DDC.Type.Exp.Simple.Predicates

          -- * Atoms
        , isXVar,       isXCon
        , isAtomX,      isAtomW

          -- * Lambdas
        , isXLAM, isXLam
        , isLambdaX

          -- * Applications
        , isXApp

          -- * Let bindings
        , isXLet

          -- * Types and Witnesses
        , isXType
        , isXWitness

          -- * Patterns
        , isPDefault)
where
import DDC.Source.Tetra.Exp.Generic
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


-- | Check whether an expression is a `XVar` or an `XCon`, 
--   or some type or witness atom.
isAtomX :: GExp l -> Bool
isAtomX xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType    _ t    -> isAtomT t
        XWitness _ w    -> isAtomW w
        _               -> False


-- | Check whether a witness is a `WVar` or `WCon`.
isAtomW :: GWitness l -> Bool
isAtomW ww
 = case ww of
        WVar{}          -> True
        WCon{}          -> True
        _               -> False


-- Lambdas --------------------------------------------------------------------
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


-- | Check whether an expression is a spec, value, or witness abstraction.
isLambdaX :: GExp l -> Bool
isLambdaX xx
        = isXLAM xx || isXLam xx


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
        

-- Type and Witness -----------------------------------------------------------
-- | Check whether an expression is an `XType`
isXType :: GExp l -> Bool
isXType xx
 = case xx of
        XType{}         -> True
        _               -> False


-- | Check whether an expression is an `XWitness`
isXWitness :: GExp l -> Bool
isXWitness xx
 = case xx of
        XWitness{}      -> True
        _               -> False


-- Patterns -------------------------------------------------------------------
-- | Check whether an alternative is a `PDefault`.
isPDefault :: GPat l -> Bool
isPDefault PDefault     = True
isPDefault _            = False

