
module DDC.Core.Flow.Prim.TyConFlow
        ( TyConFlow      (..)
        , readTyConFlow
        , kindTyConFlow

          -- * Predicates
        , isRateNatType
        , isSeriesType
        , isRefType
        , isVectorType

          -- * Compounds
        , tTuple1
        , tTuple2
        , tTupleN
        , tVector
        , tSeries
        , tSegd
        , tSel1
        , tSel2
        , tRef
        , tWorld
        , tRateNat
        , tDown
        , tTail)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData TyConFlow


instance Pretty TyConFlow where
 ppr dc
  = case dc of
        TyConFlowTuple n        -> text "Tuple" <> int n <> text "#"
        TyConFlowVector         -> text "Vector#"
        TyConFlowSeries         -> text "Series#"
        TyConFlowSegd           -> text "Segd#"
        TyConFlowSel n          -> text "Sel"   <> int n <> text "#"
        TyConFlowRef            -> text "Ref#"
        TyConFlowWorld          -> text "World#"
        TyConFlowRateNat        -> text "RateNat#"
        TyConFlowDown n         -> text "Down"  <> int n <> text "#"
        TyConFlowTail n         -> text "Tail"  <> int n <> text "#"


-- | Read a type constructor name.
readTyConFlow :: String -> Maybe TyConFlow
readTyConFlow str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConFlowTuple arity

        | Just rest     <- stripPrefix "Down" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        = Just $ TyConFlowDown n

        | Just rest     <- stripPrefix "Tail" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        = Just $ TyConFlowTail n

        | otherwise
        = case str of
                "Vector#"       -> Just $ TyConFlowVector
                "Series#"       -> Just $ TyConFlowSeries
                "Segd#"         -> Just $ TyConFlowSegd
                "Sel1#"         -> Just $ TyConFlowSel 1
                "Ref#"          -> Just $ TyConFlowRef
                "World#"        -> Just $ TyConFlowWorld
                "RateNat#"      -> Just $ TyConFlowRateNat
                _               -> Nothing


-- Kinds ----------------------------------------------------------------------
-- | Yield the kind of a primitive type constructor.
kindTyConFlow :: TyConFlow -> Kind Name
kindTyConFlow tc
 = case tc of
        TyConFlowTuple n        -> foldr kFun kData (replicate n kData)
        TyConFlowVector         -> kData `kFun` kData
        TyConFlowSeries         -> kRate `kFun` kData `kFun` kData
        TyConFlowSegd           -> kRate `kFun` kRate `kFun` kData
        TyConFlowSel n          -> foldr kFun kData (replicate (n + 1) kRate)
        TyConFlowRef            -> kData `kFun` kData
        TyConFlowWorld          -> kData
        TyConFlowRateNat        -> kRate `kFun` kData
        TyConFlowDown{}         -> kRate `kFun` kRate
        TyConFlowTail{}         -> kRate `kFun` kRate


-- Predicates -----------------------------------------------------------------
-- | Check if some type is a fully applied type of a RateNat
isRateNatType :: Type Name -> Bool
isRateNatType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowRateNat, [_])   -> True
        _                                            -> False


-- | Check if some type is a fully applied type of a Series.
isSeriesType :: Type Name -> Bool
isSeriesType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowSeries, [_, _]) -> True
        _                                            -> False


-- | Check is some type is a fully applied type of a Ref.
isRefType :: Type Name -> Bool
isRefType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowRef, [_])       -> True
        _                                            -> False


-- | Check is some type is a fully applied type of a Vector.
isVectorType :: Type Name -> Bool
isVectorType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowVector, [_])    -> True
        _                                            -> False


-- Compounds ------------------------------------------------------------------
tTuple1 :: Type Name -> Type Name
tTuple1 tA      = tApps (tConTyConFlow (TyConFlowTuple 1)) [tA]


tTuple2 :: Type Name -> Type Name -> Type Name
tTuple2 tA tB   = tApps (tConTyConFlow (TyConFlowTuple 2)) [tA, tB]


tTupleN :: [Type Name] -> Type Name
tTupleN tys     = tApps (tConTyConFlow (TyConFlowTuple (length tys))) tys


tVector :: Type Name -> Type Name
tVector tA      = tApps (tConTyConFlow TyConFlowVector)    [tA]


tSeries :: Type Name -> Type Name -> Type Name
tSeries tK tA   = tApps (tConTyConFlow TyConFlowSeries)    [tK, tA]


tSegd :: Type Name -> Type Name -> Type Name
tSegd tK1 tK2   = tApps (tConTyConFlow TyConFlowSegd)      [tK1, tK2]


tSel1 :: Type Name -> Type Name -> Type Name
tSel1 tK1 tK2   = tApps (tConTyConFlow $ TyConFlowSel 1) [tK1, tK2]


tSel2 :: Type Name -> Type Name -> Type Name -> Type Name
tSel2 tK1 tK2 tK3 = tApps (tConTyConFlow $ TyConFlowSel 2) [tK1, tK2, tK3]


tRef  :: Type Name -> Type Name
tRef tVal       = tApp (tConTyConFlow $ TyConFlowRef) tVal


tWorld :: Type Name
tWorld          = tConTyConFlow TyConFlowWorld


tRateNat :: Type Name -> Type Name
tRateNat tK     = tApp (tConTyConFlow TyConFlowRateNat)  tK


tDown :: Int -> Type Name -> Type Name 
tDown n tK      = tApp (tConTyConFlow $ TyConFlowDown n) tK


tTail :: Int -> Type Name -> Type Name 
tTail n tK      = tApp (tConTyConFlow $ TyConFlowTail n) tK


-- Utils ----------------------------------------------------------------------
tConTyConFlow :: TyConFlow -> Type Name
tConTyConFlow tcf
 = let  k       = kindTyConFlow tcf
        u       = UPrim (NameTyConFlow tcf) k
        tc      = TyConBound u k
   in   TCon tc

