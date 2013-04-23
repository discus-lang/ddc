
module DDC.Core.Flow.Prim.DaConPrim
        ( xNat
        , dcNat)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.DaCon
import DDC.Core.Exp


-- | A literal Nat#
xNat  :: a -> Integer -> Exp a Name
xNat a i = XCon a (dcNat i)


-- | A Literal Nat# data constructor.
dcNat :: Integer -> DaCon Name
dcNat i = mkDaConAlg (NameLitNat i) tNat

