
module DDC.Core.Tetra.Prim.OpStore
        ( readOpStore
        , typeOpStore)
where
import DDC.Core.Tetra.Prim.TyConData
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List


-- OpStore --------------------------------------------------------------------
instance NFData OpStore

instance Pretty OpStore where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) opStoreNames
    in  (text n)


-- | Read a primitive store operator.
readOpStore :: String -> Maybe OpStore
readOpStore str
  =  case find (\(_, n) -> str == n) opStoreNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitive store operators.
opStoreNames :: [(OpStore, String)]
opStoreNames
 =      [ (OpStoreAllocRef,   "allocRef#")
        , (OpStoreReadRef,    "readRef#")
        , (OpStoreWriteRef,   "writeRef#") ]


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
