
-- | Simple predicates on core expressions.
module DDC.Core.Predicates
        ( isXLam
        , isPDefault)
where
import DDC.Core.Exp


-- | Check whether an expression is a Lambda abstraction.
isXLam :: Exp a n -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False


-- | Check whether an alternative is the default alternative.
isPDefault :: Pat n -> Bool
isPDefault PDefault     = True
isPDefault _            = False
