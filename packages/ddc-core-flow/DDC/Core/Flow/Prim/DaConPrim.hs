
module DDC.Core.Flow.Prim.DaConPrim
        ( xBool, dcBool
        , xNat,  dcNat
        , dcTuple1
        , xTuple2, dcTuple2
        , dcTupleN)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.DaConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple


-- | A literal @Bool#@
xBool   :: Bool   -> Exp a Name
xBool b  = XCon (dcBool b)


-- | A literal @Bool#@ data constructor.
dcBool  :: Bool -> DaCon Name
dcBool b = DaConPrim (NameLitBool b) tBool


-- | A literal @Nat#@
xNat    :: Integer -> Exp a Name
xNat i  = XCon (dcNat i)


-- | A Literal @Nat#@ data constructor.
dcNat   :: Integer -> DaCon Name
dcNat i   = DaConPrim (NameLitNat i) tNat


-- | Data constructor for @Tuple1#@
dcTuple1 :: DaCon Name
dcTuple1  = DaConPrim (NameDaConFlow (DaConFlowTuple 1))
                      (typeDaConFlow (DaConFlowTuple 1))


-- | Construct a @Tuple2#@
xTuple2 :: Type Name  -> Type Name 
        -> Exp a Name -> Exp a Name 
        -> Exp a Name

xTuple2 t1 t2 x1 x2
        = xApps (XCon dcTuple2) 
                [XType t1, XType t2, x1, x2]


-- | Data constructor for @Tuple2#@
dcTuple2 :: DaCon Name
dcTuple2  = DaConPrim   (NameDaConFlow (DaConFlowTuple 2))
                        (typeDaConFlow (DaConFlowTuple 2))


-- | Data constructor for n-tuples
dcTupleN :: Int -> DaCon Name
dcTupleN n
          = DaConPrim   (NameDaConFlow (DaConFlowTuple n))
                        (typeDaConFlow (DaConFlowTuple n))

