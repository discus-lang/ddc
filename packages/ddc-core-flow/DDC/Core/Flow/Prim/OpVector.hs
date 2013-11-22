module DDC.Core.Flow.Prim.OpVector
        ( readOpVector
        , typeOpVector
        )
where
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char        


instance NFData OpVector


instance Pretty OpVector where
 ppr pf
  = case pf of
        OpVectorMap 1             -> text "vmap"                  <> text "#"
        OpVectorMap i             -> text "vmap"       <> int i   <> text "#"

        OpVectorFilter            -> text "vfilter"               <> text "#"

        OpVectorReduce            -> text "vreduce"               <> text "#"

        OpVectorGenerate          -> text "vgenerate"             <> text "#"
        OpVectorLength            -> text "vlength"               <> text "#"


-- | Read a data flow operator name.
readOpVector :: String -> Maybe OpVector
readOpVector str
        | Just rest     <- stripPrefix "vmap" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpVectorMap arity

        | otherwise
        = case str of
                "vmap#"         -> Just $ OpVectorMap   1
                "vfilter#"      -> Just $ OpVectorFilter
                "vreduce#"      -> Just $ OpVectorReduce
                "vgenerate#"    -> Just $ OpVectorGenerate
                "vlength#"      -> Just $ OpVectorLength
                _               -> Nothing


-- Types -----------------------------------------------------------------------
-- | Yield the type of a data flow operator, 
--   or `error` if there isn't one.
typeOpVector :: OpVector -> Type Name
typeOpVector op
 = case takeTypeOpVector op of
        Just t  -> t
        Nothing -> error $ "ddc-core-flow.typeOpVector: invalid op " ++ show op


-- | Yield the type of a data flow operator.
takeTypeOpVector :: OpVector -> Maybe (Type Name)
takeTypeOpVector op
 = case op of
        -- Maps ---------------------------------
        -- map   :: [a b : Data]
        --       .  (a -> b) -> Vector a -> Vector b
        OpVectorMap 1
         -> Just $ tForalls [kData, kData] $ \[tA, tB]
                ->       (tA `tFun` tB)
                `tFun` tVector tA
                `tFun` tVector tB

        -- mapN  :: [a0..aN : Data]
        --       .  (a0 -> .. aN) -> Vector a0 -> .. Vector aN
        OpVectorMap n
         | n >= 2
         , Just tWork <- tFunOfList   
                         [ TVar (UIx i) 
                                | i <- reverse [0..n] ]

         , Just tBody <- tFunOfList
                         (tWork : [tVector (TVar (UIx i)) 
                                | i <- reverse [0..n] ])

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- replicate (n + 1) kData ]

        -- Selectors ----------------------------
        -- filter#    :: [a : Data]
        --            .  Vector a
        --            -> (a -> Bool#)
        --            -> Vector a
        OpVectorFilter
         -> Just $ tForalls [kData] $ \[tA]
                ->     (tA `tFun` tBool)
                `tFun` tVector tA
                `tFun` tVector tA

        -- reduce#   :: [a: Data]
        --           .  (a -> a -> a) -> a -> Vector a -> a
        OpVectorReduce
         -> Just $ tForalls [kData] $ \[tA]
                 ->     (tA `tFun` tA `tFun` tA)
                 `tFun` tA
                 `tFun` tVector tA
                 `tFun` tA

        -- Vector creation and filling ----------
        -- generate :: [a : Data]. Nat# -> (Nat# -> a) -> Vector a
        OpVectorGenerate
         -> Just $ tForalls [kData] $ \[tA] 
                ->     tNat
                `tFun` (tNat `tFun` tA)
                `tFun` tVector tA

        -- length   :: [a : Data]. Vector a -> Nat#
        OpVectorLength
         -> Just $ tForalls [kData] $ \[tA] 
                -> tVector tA `tFun` tNat

        _ -> Nothing

