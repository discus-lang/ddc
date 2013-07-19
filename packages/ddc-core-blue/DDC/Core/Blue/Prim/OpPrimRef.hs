module DDC.Core.Blue.Prim.OpPrimRef
        ( readOpPrimRef
        , typeOpPrimRef)
where
import DDC.Core.Blue.Prim.TyConPrim
import DDC.Core.Blue.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List


-- OpPrimArith ----------------------------------------------------------------
instance NFData OpPrimRef

instance Pretty OpPrimRef where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) opPrimRefNames
    in  (text n)


-- | Read a primitive operator.
readOpPrimRef :: String -> Maybe OpPrimRef
readOpPrimRef str
  =  case find (\(_, n) -> str == n) opPrimRefNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
opPrimRefNames :: [(OpPrimRef, String)]
opPrimRefNames
 =      [ (OpPrimRefAllocRef,   "allocRef#")
        , (OpPrimRefReadRef,    "readRef#")
        , (OpPrimRefWriteRef,   "writeRef#") ]


-- | Take the type of a primitive arithmetic operator.
typeOpPrimRef :: OpPrimRef -> Type Name
typeOpPrimRef op
 = case op of
        OpPrimRefAllocRef  
         -> tForalls [kRegion, kData] 
          $ \[tR, tA] -> tA 
                        `tFun` tSusp (tAlloc tR) (tRef tR tA)

        OpPrimRefReadRef   
         -> tForalls [kRegion, kData]
          $ \[tR, tA] -> tRef tR tA
                        `tFun` tSusp (tRead tR) tA

        OpPrimRefWriteRef  
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tRef tR tA `tFun` tA
                        `tFun` tSusp (tWrite tR) tUnit
