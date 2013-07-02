
module DDC.Core.Flow.Prim.TyConFlow
        ( TyConFlow      (..)
        , readTyConFlow
        , kindTyConFlow
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
        , tRateNat)
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



-- | Read a type constructor name.
readTyConFlow :: String -> Maybe TyConFlow
readTyConFlow str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConFlowTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just $ TyConFlowVector
                "Series#"       -> Just $ TyConFlowSeries

                "Segd#"         -> Just $ TyConFlowSegd
                "Sel1#"         -> Just $ TyConFlowSel 1
                "Sel2#"         -> Just $ TyConFlowSel 2

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
        TyConFlowSel 1          -> kRate `kFun` kRate `kFun` kData
        TyConFlowSel 2          -> kRate `kFun` kRate `kFun` kRate `kFun` kData
        TyConFlowRef            -> kData `kFun` kData
        TyConFlowWorld          -> kData
        TyConFlowRateNat        -> kRate `kFun` kData
        _ -> error $ "ddc-core-flow.kindTyConFlow: no match for " ++ show tc


-- Compounds ------------------------------------------------------------------
tTuple1 :: Type Name -> Type Name
tTuple1 tA      = tApps (tConTyConFlow (TyConFlowTuple 1)) [tA]


tTuple2 :: Type Name -> Type Name -> Type Name
tTuple2 tA tB   = tApps (tConTyConFlow (TyConFlowTuple 2)) [tA, tB]


tTupleN :: [Type Name] -> Type Name
tTupleN tys     = tApps (tConTyConFlow (TyConFlowTuple (length tys))) tys


tVector :: Type Name -> Type Name
tVector tA      = tApps (tConTyConFlow TyConFlowVector)   [tA]


tSeries :: Type Name -> Type Name -> Type Name
tSeries tK tA   = tApps (tConTyConFlow TyConFlowSeries)   [tK, tA]


tSegd :: Type Name -> Type Name -> Type Name
tSegd tK1 tK2   = tApps (tConTyConFlow TyConFlowSegd)     [tK1, tK2]


tSel1 :: Type Name -> Type Name -> Type Name
tSel1 tK1 tK2     = tApps (tConTyConFlow $ TyConFlowSel 1)  [tK1, tK2]


tSel2 :: Type Name -> Type Name -> Type Name -> Type Name
tSel2 tK1 tK2 tK3 = tApps (tConTyConFlow $ TyConFlowSel 2) [tK1, tK2, tK3]


tRef  :: Type Name -> Type Name
tRef tVal       = tApp (tConTyConFlow $ TyConFlowRef) tVal


tWorld :: Type Name
tWorld          = tConTyConFlow TyConFlowWorld


tRateNat :: Type Name -> Type Name
tRateNat tK     = tApps (tConTyConFlow TyConFlowRateNat) [tK]


-- Utils ----------------------------------------------------------------------
tConTyConFlow :: TyConFlow -> Type Name
tConTyConFlow tcf
 = let  k       = kindTyConFlow tcf
        u       = UPrim (NameTyConFlow tcf) k
        tc      = TyConBound u k
   in   TCon tc

