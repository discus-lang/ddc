
-- | Simple predicates on core expressions.
module DDC.Core.Predicates
        ( isXLAM
        , isXLam
        , isXLambda
        , isXApp
        , isPDefault)
where
import DDC.Core.Exp


-- | Check whether an expression is a Lambda abstraction.
isXLAM :: Exp a n -> Bool
isXLAM xx
 = case xx of
        XLAM{}  -> True
        _       -> False


-- | Check whether an expression is a Lambda abstraction.
isXLam :: Exp a n -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False


-- | Check whether an expression is an XLAM or XLam
isXLambda :: Exp a n -> Bool
isXLambda xx
        = isXLAM xx || isXLam xx


-- | Check whether an expression is an XApp
isXApp :: Exp a n -> Bool
isXApp xx
 = case xx of
        XApp{}  -> True
        _       -> False


-- | Check whether an alternative is the default alternative.
isPDefault :: Pat n -> Bool
isPDefault PDefault     = True
isPDefault _            = False
