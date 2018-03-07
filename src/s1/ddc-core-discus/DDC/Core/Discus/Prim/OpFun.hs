
module DDC.Core.Discus.Prim.OpFun
        ( OpFun (..)
        , readOpFun
        , typeOpFun)
where
import DDC.Core.Discus.Prim.TyConDiscus
import DDC.Core.Discus.Prim.Base
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData OpFun where
 rnf op
  = case op of
        OpFunCurry   n  -> rnf n
        OpFunApply   n  -> rnf n
        OpFunCReify     -> ()


instance Pretty OpFun where
 ppr pf
  = case pf of
        OpFunCurry  n
         -> text "curry"   <> int n <> text "#"

        OpFunApply  n
         -> text "apply"   <> int n <> text "#"

        OpFunCReify
         -> text "creify#"


-- | Read a primitive function operator.
readOpFun :: String -> Maybe OpFun
readOpFun str
        -- curryN#
        | Just rest     <- stripPrefix "curry" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ OpFunCurry n

        -- applyN#
        | Just rest     <- stripPrefix "apply" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpFunApply n

        -- creify#
        | "creify#"      <- str
        = Just $ OpFunCReify

        | otherwise
        = Nothing


-- | Take the type of a primitive function operator.
typeOpFun :: OpFun -> Type Name
typeOpFun op
 = case op of
        OpFunCurry n
         -> tForalls (replicate (n + 1) kData)
         $  \ts ->
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result
                        = tFunOfList
                                ( tFunValue tF
                                : tsFront ++ [tLast])
                in  result

        OpFunApply n
         -> tForalls (replicate (n + 1) kData)
         $  \ts ->
                let Just tF          = tFunOfList ts
                    Just result      = tFunOfList (tF : ts)
                in  result

        OpFunCReify
         -> tForalls [kData, kData]
         $  \[tA, tB]  -> (tA `tFun` tB) `tFun` tFunValue (tA `tFun` tB)
