
module DDC.Core.Flow.Prim.DaConPrim
        ( xNat
        , dcNat
        , xTuple2, dcTuple2
        , dcTuple1)
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


-- | Data constructor for Tuple1#
dcTuple1 :: DaCon Name
dcTuple1  = mkDaConAlg (NameDaConFlow (DaConFlowTuple 1))
          $ typeDaConFlow (DaConFlowTuple 1)


-- | Construct a Tuple2#
xTuple2 :: a 
        -> Type Name  -> Type Name 
        -> Exp a Name -> Exp a Name 
        -> Exp a Name

xTuple2 a t1 t2 x1 x2
        = xApps a (XCon a dcTuple2) 
                  [XType t1, XType t2, x1, x2]


-- | Data constructor for Tuple2#
dcTuple2 :: DaCon Name
dcTuple2  = mkDaConAlg (NameDaConFlow (DaConFlowTuple 2))
          $ typeDaConFlow (DaConFlowTuple 2)
