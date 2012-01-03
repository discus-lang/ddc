
module DDC.Core.Predicates
        ( isXLam
        , isPDefault)
where
import DDC.Core.Exp

isXLam :: Exp a n -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False

isPDefault :: Pat n -> Bool
isPDefault PDefault     = True
isPDefault _            = False
