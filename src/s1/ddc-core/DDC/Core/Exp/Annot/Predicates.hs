{-# OPTIONS_HADDOCK hide #-}
-- | Simple predicates on core expressions.
module DDC.Core.Exp.Annot.Predicates
        ( module DDC.Type.Exp.Simple.Predicates

          -- * Atoms
        , isXVar,  isXCon
        , isAtomX, isAtomW

          -- * Lambdas
        , isXLAM, isXLam
        , isLambdaX

          -- * Applications
        , isXApp

          -- * Arguments
        , isRType
        , isRWitness
        , isRTerm
        , isRImplicit

          -- * Cast
        , isXCast
        , isXCastBox
        , isXCastRun

          -- * Let bindings
        , isXLet

          -- * Patterns
        , isPDefault)
where
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Exp.Simple.Predicates


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
        XVar{}  -> True
        XCon{}  -> True
        _       -> False


-- | Check whether a witness is a `WVar` or `WCon`.
isAtomW :: Witness a n -> Bool
isAtomW ww
 = case ww of
        WVar{}  -> True
        WCon{}  -> True
        _       -> False


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


-- Arguments ------------------------------------------------------------------
-- | Check whether this argument is an `RType`.
isRType :: Arg a n -> Bool
isRType aa
 = case aa of
        RType{}         -> True
        _               -> False


-- | Check whether this argument is an `RWitness`.
isRWitness :: Arg a n -> Bool
isRWitness aa
 = case aa of
        RWitness{}      -> True
        _               -> False


-- | Check whether this argument is an `RTerm`.
isRTerm :: Arg a n -> Bool
isRTerm aa
 = case aa of
        RTerm{}         -> True
        _               -> False


-- | Check whether this argument is an `RImplicit`.
isRImplicit :: Arg a n -> Bool
isRImplicit aa
 = case aa of
        RImplicit{}     -> True
        _               -> False


-- Casts ----------------------------------------------------------------------
-- | Check whether this is a cast expression.
isXCast :: Exp a n -> Bool
isXCast xx
 = case xx of
        XCast{}         -> True
        _               -> False


-- | Check whether this is a box cast.
isXCastBox :: Exp a n -> Bool
isXCastBox xx
 = case xx of
        XCast _ CastBox _ -> True
        _                 -> False


-- | Check whether this is a run cast.
isXCastRun :: Exp a n -> Bool
isXCastRun xx
 = case xx of
        XCast _ CastRun _ -> True
        _                 -> False


-- Let Bindings ---------------------------------------------------------------
-- | Check whether an expression is a `XLet`.
isXLet :: Exp a n -> Bool
isXLet xx
 = case xx of
        XLet{}          -> True
        _               -> False


-- Patterns -------------------------------------------------------------------
-- | Check whether an alternative is a `PDefault`.
isPDefault :: Pat n -> Bool
isPDefault PDefault     = True
isPDefault _            = False

