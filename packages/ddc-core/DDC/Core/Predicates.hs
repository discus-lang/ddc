
-- | Simple predicates on core expressions.
module DDC.Core.Predicates
        ( module DDC.Type.Predicates

          -- * Atoms
        , isXVar,  isXCon
        , isAtomX, isAtomW

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
import DDC.Core.Exp
import DDC.Type.Predicates


-- Atoms ----------------------------------------------------------------------
-- | Check whether an expression is a variable.
isXVar :: Exp a n -> Bool
isXVar xx
 = case xx of
        XVar{}  -> True
        _       -> False


-- | Check whether an expression is a constructor.
isXCon :: Exp a n -> Bool
isXCon xx
 = case xx of
        XCon{}  -> True
        _       -> False


-- | Check whether an expression is a `XVar` or an `XCon`, 
--   or some type or witness atom.
isAtomX :: Exp a n -> Bool
isAtomX xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType t         -> isAtomT t
        XWitness w      -> isAtomW w
        _               -> False


-- | Check whether a witness is a `WVar` or `WCon`.
isAtomW :: Witness n -> Bool
isAtomW ww
 = case ww of
        WVar{}          -> True
        WCon{}          -> True
        _               -> False


-- Lambdas --------------------------------------------------------------------
-- | Check whether an expression is a spec abstraction (level-1).
isXLAM :: Exp a n -> Bool
isXLAM xx
 = case xx of
        XLAM{}  -> True
        _       -> False


-- | Check whether an expression is a value or witness abstraction (level-0).
isXLam :: Exp a n -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False


-- | Check whether an expression is a spec, value, or witness abstraction.
isLambdaX :: Exp a n -> Bool
isLambdaX xx
        = isXLAM xx || isXLam xx


-- Applications ---------------------------------------------------------------
-- | Check whether an expression is an `XApp`.
isXApp :: Exp a n -> Bool
isXApp xx
 = case xx of
        XApp{}  -> True
        _       -> False


-- Let Bindings ---------------------------------------------------------------
isXLet :: Exp a n -> Bool
isXLet xx
 = case xx of
        XLet{}  -> True
        _       -> False
        

-- Type and Witness -----------------------------------------------------------
-- | Check whether an expression is an `XType`
isXType :: Exp a n -> Bool
isXType xx
 = case xx of
        XType{}         -> True
        _               -> False


-- | Check whether an expression is an `XWitness`
isXWitness :: Exp a n -> Bool
isXWitness xx
 = case xx of
        XWitness{}      -> True
        _               -> False


-- Patterns -------------------------------------------------------------------
-- | Check whether an alternative is a `PDefault`.
isPDefault :: Pat n -> Bool
isPDefault PDefault     = True
isPDefault _            = False

