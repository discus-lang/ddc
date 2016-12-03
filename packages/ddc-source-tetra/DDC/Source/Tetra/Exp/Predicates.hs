
-- | Simple predicates on Source Tetra things.
module DDC.Source.Tetra.Exp.Predicates
        ( module DDC.Type.Exp.Generic.Predicates

          -- * Atoms
        , isXVar,       isXCon
        , isAtomX,      isAtomW

          -- * Abstractions
        , isXAbs

          -- * Applications
        , isXApp

          -- * Let bindings
        , isXLet

          -- * Types and Witnesses
        , isXType
        , isXWitness

          -- * Patterns
        , isPDefault
        , isPVar)
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Type.Exp.Generic.Predicates


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
        XType    t      -> isAtomT t
        XWitness w      -> isAtomW w
        _               -> False


-- | Check whether a witness is a `WVar` or `WCon`.
isAtomW :: GWitness l -> Bool
isAtomW ww
 = case ww of
        WVar{}          -> True
        WCon{}          -> True
        _               -> False


-- Abstractions----------------------------------------------------------------
-- | Check whether an expression is an abstraction.
isXAbs :: GExp l -> Bool
isXAbs xx
 = case xx of
        XAbs{}  -> True
        _       -> False

{-
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
-}

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
-- | Check whether a pattern is a `PDefault`.
isPDefault :: GPat l -> Bool
isPDefault PDefault     = True
isPDefault _            = False


-- | Check whether a pattern is a `PVar`.
isPVar     :: GPat l -> Bool
isPVar (PVar _)         = True
isPVar _                = False

