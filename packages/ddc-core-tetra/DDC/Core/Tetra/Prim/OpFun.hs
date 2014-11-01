
module DDC.Core.Tetra.Prim.OpFun
        ( OpFun (..)
        , readOpFun
        , typeOpFun)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData OpFun where
 rnf prim
  = case prim of
        OpFunCReify   -> ()
        OpFunCCurry n -> rnf n
        OpFunCApply n -> rnf n
        OpFunCEval  n -> rnf n
 

instance Pretty OpFun where
 ppr pf
  = case pf of
        OpFunCReify
         -> text "creify#"

        OpFunCCurry n
         -> text "ccurry" <> int n <> text "#"

        OpFunCApply n
         -> text "capply" <> int n <> text "#"

        OpFunCEval  n
         -> text "ceval"  <> int n <> text "#"


-- | Read a primitive function operator.
readOpFun :: String -> Maybe OpFun
readOpFun str

        -- creify#
        | "creify#"      <- str
        = Just $ OpFunCReify

        -- ccurryN#
        | Just rest     <- stripPrefix "ccurry" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ OpFunCCurry n

        -- capplyN#
        | Just rest     <- stripPrefix "capply" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpFunCApply n

        -- cevalN#
        | Just rest     <- stripPrefix "ceval" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ OpFunCEval n

        | otherwise
        = Nothing


-- | Take the type of a primitive function operator.
typeOpFun :: OpFun -> Type Name
typeOpFun op
 = case op of
        OpFunCReify
         -> tForalls [kData, kData]
         $  \[tA, tB]  -> (tA `tFun` tB) `tFun` tFunValue (tA `tFun` tB)

        OpFunCCurry n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result         
                        = tFunOfList 
                                ( tFunValue tF
                                : tsFront ++ [tCloValue tLast])
                in result

        OpFunCApply n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result
                        = tFunOfList
                                ( tCloValue tF
                                : tsFront ++ [tCloValue tLast])
                in result

        OpFunCEval n
         -> tForalls (replicate (n + 1) kData)
         $  \ts ->
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result
                        = tFunOfList
                                ( tCloValue tF
                                : tsFront ++ [tLast])
                in result

