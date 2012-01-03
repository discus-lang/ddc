
module DDC.Core.Predicates
        (isXLam)
where
import DDC.Core.Exp

isXLam :: Exp a n -> Bool
isXLam xx
 = case xx of
        XLam{}  -> True
        _       -> False
