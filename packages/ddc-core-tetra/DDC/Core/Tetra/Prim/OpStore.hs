
module DDC.Core.Tetra.Prim.OpStore
        ( readOpStore
        , typeOpStore)
where
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData OpStore

instance Pretty OpStore where
 ppr (OpStoreProj i j)
  = text ("proj" ++ show i ++ "_" ++ show j ++ "#")
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) opStoreNames
    in  (text n)


-- | Read a primitive store operator.
readOpStore :: String -> Maybe OpStore
readOpStore str
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
  = Just $ OpStoreProj arity ix

  | otherwise
  =  case find (\(_, n) -> str == n) opStoreNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitive store operators.
opStoreNames :: [(OpStore, String)]
opStoreNames
 =      [ (OpStoreAllocRef,   "allocRef#")
        , (OpStoreReadRef,    "readRef#")
        , (OpStoreWriteRef,   "writeRef#")
        , (OpStoreAllocPtr,   "allocPtr#")
        , (OpStoreReadPtr,    "readPtr#")
        , (OpStoreWritePtr,   "writePtr#") ]


-- | Take the type of a primitive store operator.
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        OpStoreAllocRef  
         -> tForalls [kRegion, kData] 
          $ \[tR, tA] -> tA 
                        `tFun` tSusp (tAlloc tR) (tRef tR tA)

        OpStoreReadRef   
         -> tForalls [kRegion, kData]
          $ \[tR, tA] -> tRef tR tA
                        `tFun` tSusp (tRead tR) tA

        OpStoreWriteRef  
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tRef tR tA `tFun` tA
                        `tFun` tSusp (tWrite tR) tUnit

        OpStoreAllocPtr
         -> tForalls [kRegion, kData] 
          $ \[tR, tA] -> tNat
                        `tFun` tSusp (tAlloc tR) (tPtr tR tA)

        OpStoreReadPtr   
         -> tForalls [kRegion, kData, kData]
          $ \[tR, tA, tO] 
          -> tPtr tR tA `tFun` tO
                        `tFun` tSusp (tRead tR) tA

        OpStoreWritePtr  
         -> tForalls [kRegion, kData, kData]
         $  \[tR, tA, tO] 
         -> tPtr tR tA `tFun` tO `tFun` tA
                       `tFun` tSusp (tWrite tR) tUnit

        -- Tuple projections --------------------
        OpStoreProj a ix
         -> tForalls (replicate a kData) 
         $ \_ -> tFun   (tTupleN [TVar (UIx i) | i <- reverse [0..a-1]])
                        (TVar (UIx (a - ix)))

