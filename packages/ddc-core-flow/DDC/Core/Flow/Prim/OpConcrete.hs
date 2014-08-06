
module DDC.Core.Flow.Prim.OpConcrete
        ( readOpConcrete
        , typeOpConcrete

        -- * Compounds
        , xProj
        , xRateOfSeries
        , xNatOfRateNat
        , xNext
        , xNextC

        , xDown
        , xTail
        , xRunKernel)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


instance NFData OpConcrete


instance Pretty OpConcrete where
 ppr pf
  = case pf of
        OpConcreteProj arity ix   -> text "proj" 
                                        <> int arity <> text "_" <> int ix
                                        <> text "#"

        OpConcreteRateOfSeries    -> text "rateOfSeries"  <> text "#"
        OpConcreteNatOfRateNat    -> text "natOfRateNat"  <> text "#"

        OpConcreteNext 1          -> text "next#"
        OpConcreteNext n          -> text "next$"         <> int n <> text "#"

        OpConcreteDown n          -> text "down$"         <> int n <> text "#"
        OpConcreteTail n          -> text "tail$"         <> int n <> text "#"

        OpConcreteRunKernel 1     -> text "runKernel"              <> text "#"
        OpConcreteRunKernel n     -> text "runKernel"     <> int n <> text "#"


-- | Read a series operator name.
readOpConcrete :: String -> Maybe OpConcrete
readOpConcrete str
        | Just rest         <- stripPrefix "proj" str
        , (ds, '_' : rest2) <- span isDigit rest
        , not $ null ds
        , arity             <- read ds
        , arity >= 1
        , (ds2, "#")        <- span isDigit rest2
        , not $ null ds2
        , ix                <- read ds2
        , ix >= 1
        , ix <= arity
        = Just $ OpConcreteProj arity ix


        | Just rest     <- stripPrefix "next$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpConcreteNext n

        | Just rest     <- stripPrefix "down$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpConcreteDown n

        | Just rest     <- stripPrefix "tail$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpConcreteTail n

        | Just rest     <- stripPrefix "runKernel" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpConcreteRunKernel n

        | otherwise
        = case str of
                "rateOfSeries#" -> Just $ OpConcreteRateOfSeries
                "natOfRateNat#" -> Just $ OpConcreteNatOfRateNat
                "next#"         -> Just $ OpConcreteNext 1
                "runKernel#"    -> Just $ OpConcreteRunKernel 1
                _               -> Nothing


-- | Yield the type of a series operator.
typeOpConcrete :: OpConcrete -> Type Name
typeOpConcrete op
 = case op of
        -- Tuple projections --------------------
        OpConcreteProj a ix
         -> tForalls (replicate a kData) 
         $ \_ -> tFun   (tTupleN [TVar (UIx i) | i <- reverse [0..a-1]])
                        (TVar (UIx (a - ix)))


        -- rateOfSeries#   :: [k : Rate]. [a : Data]
        --                 .  Series k a -> RateNat k
        OpConcreteRateOfSeries 
         -> tForalls [kRate, kData] $ \[tK, tA]
                -> tSeries tK tA `tFun` tRateNat tK

        -- natOfRateNat#   :: [k : Rate]. RateNat k -> Nat#
        OpConcreteNatOfRateNat 
         -> tForall kRate $ \tK 
                -> tRateNat tK `tFun` tNat

        -- next#   :: [a : Data]. [k : Rate]. Series# k a -> Nat# -> a
        OpConcreteNext 1
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFun` tNat `tFun` tA

        -- next$N# :: [a : Data]. [k : Rate]
        --         .  Series# (DownN# k) a -> Nat# -> VecN# a
        OpConcreteNext n
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries (tDown n tK) tA `tFun` tNat `tFun` tVec n tA

        -- down$N# :: [k : Rate]. [a : Data].
        --         .  RateNat (DownN# k) -> Series# k a -> Series# (DownN# k) a
        OpConcreteDown n
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tRateNat (tDown n tK) 
                        `tFun` tSeries tK tA `tFun` tSeries (tDown n tK) tA

        -- tail$N# :: [k : Rate]. [a : Data].
        --         .  RateNat (TailN# k) -> Series# k a -> Series# (TailN# k) a
        OpConcreteTail n
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tRateNat (tTail n tK)
                        `tFun` tSeries tK tA `tFun` tSeries (tTail n tK) tA

        -- runKernelN# :: [a0..aN : Data]
        --          .  Vector    a0 .. Vector   aN 
        --          -> ([k : Rate]. RateNat k -> Series k a0 .. Series k aN -> Unit)
        --          -> Bool
        OpConcreteRunKernel n
         -> let tK     = TVar (UIx 0)
                tWork  = foldr tFun tUnit
                       $ [ tRateNat tK ]
                       ++[ tSeries tK (TVar (UIx i))
                                | i <- reverse [1..n] ]

                tWork' = TForall (BAnon kRate) tWork
                
                tBody  = foldr tFun tBool
                       $ [ tVector (TVar (UIx i)) | i <- reverse [0..n-1] ]
                       ++[ tWork' ]
            in  foldr TForall tBody [ BAnon k | k <- replicate n kData ]


-- Compounds ------------------------------------------------------------------
type TypeF      = Type Name
type ExpF       = Exp () Name

xProj :: [Type Name] -> Int -> Exp () Name -> Exp () Name
xProj ts ix  x
        = xApps   (xVarOpConcrete (OpConcreteProj (length ts) ix))
                  ([XType t | t <- ts] ++ [x])


xRateOfSeries :: TypeF -> TypeF -> ExpF -> ExpF
xRateOfSeries tK tA xS 
         = xApps  (xVarOpConcrete OpConcreteRateOfSeries) 
                  [XType tK, XType tA, xS]


xNatOfRateNat :: TypeF -> ExpF -> ExpF
xNatOfRateNat tK xR
        = xApps  (xVarOpConcrete OpConcreteNatOfRateNat)
                 [XType tK, xR]


xNext  :: TypeF -> TypeF -> ExpF -> ExpF -> ExpF
xNext tRate tElem xStream xIndex
 = xApps (xVarOpConcrete (OpConcreteNext 1))
         [XType tElem, XType tRate, xStream, xIndex]


xNextC :: Int -> TypeF -> TypeF -> ExpF -> ExpF -> ExpF
xNextC c tRate tElem xStream xIndex
 = xApps (xVarOpConcrete (OpConcreteNext c))
         [XType tElem, XType tRate, xStream, xIndex]


xDown  :: Int -> TypeF -> TypeF -> ExpF -> ExpF -> ExpF
xDown n tR tE xRN xS
 = xApps (xVarOpConcrete (OpConcreteDown n))
         [XType tR, XType tE, xRN, xS]


xTail  :: Int -> TypeF -> TypeF -> ExpF -> ExpF -> ExpF
xTail n tR tE xRN xS
 = xApps (xVarOpConcrete (OpConcreteTail n))
         [XType tR, XType tE, xRN, xS]


xRunKernel  :: Int -> ExpF
xRunKernel n
 = xVarOpConcrete (OpConcreteRunKernel n)


-- Utils -----------------------------------------------------------------------
xVarOpConcrete :: OpConcrete -> Exp () Name
xVarOpConcrete op
        = XVar  (UPrim (NameOpConcrete op) (typeOpConcrete op))

