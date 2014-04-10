
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
        OpFunReify   -> ()
        OpFunCurry n -> rnf n
        OpFunApply n -> rnf n
        OpFunEval  n -> rnf n
 

instance Pretty OpFun where
 ppr pf
  = case pf of
        OpFunReify
         -> text "reify#"

        OpFunCurry n
         -> text "curry" <> int n <> text "#"

        OpFunApply n
         -> text "apply" <> int n <> text "#"

        OpFunEval  n
         -> text "eval"  <> int n <> text "#"


-- | Read a primitive function operator.
readOpFun :: String -> Maybe OpFun
readOpFun str

        -- reify#
        | "reify#"      <- str
        = Just $ OpFunReify

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

        -- evalN#
        | Just rest     <- stripPrefix "eval" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ OpFunEval n

        | otherwise
        = Nothing


-- | Take the type of a primitive function operator.
typeOpFun :: OpFun -> Type Name
typeOpFun op
 = case op of
        OpFunReify
         -> tForalls [kData, kData]
         $  \[tA, tB]  -> (tA `tFun` tB) `tFun` tFunValue (tA `tFun` tB)

        OpFunCurry n
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

        OpFunApply n
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

        OpFunEval n
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

