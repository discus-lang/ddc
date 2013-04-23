
module DDC.Core.Flow.Prim.DaConPrim
        ( xNat
        , dcNat
        , xTuple2
        , dcTuple2)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp


-- | A literal Nat#
xNat  :: a -> Integer -> Exp a Name
xNat a i = XCon a (dcNat i)


-- | A Literal Nat# data constructor.
dcNat :: Integer -> DaCon Name
dcNat i   = mkDaConAlg (NameLitNat i) tNat



-- | Construct a Tuple2#
xTuple2 :: a -> Exp a Name -> Exp a Name -> Exp a Name
xTuple2 a x1 x2
        = xApps a (XCon a dcTuple2) [x1, x2]

-- | Data constructor for Tuple2#
dcTuple2 :: DaCon Name
dcTuple2  = mkDaConAlg (NameDaConFlow (DaConFlowTuple 2))
          $ typeDaConFlow (DaConFlowTuple 2)
