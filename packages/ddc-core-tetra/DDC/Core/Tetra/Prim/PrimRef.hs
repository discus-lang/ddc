
module DDC.Core.Tetra.Prim.PrimRef
        ( readPrimRef
        , typePrimRef)
where
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List


-- OpPrimArith ----------------------------------------------------------------
instance NFData PrimRef

instance Pretty PrimRef where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) opPrimRefNames
    in  (text n)


-- | Read a primitive operator.
readPrimRef :: String -> Maybe PrimRef
readPrimRef str
  =  case find (\(_, n) -> str == n) opPrimRefNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
opPrimRefNames :: [(PrimRef, String)]
opPrimRefNames
 =      [ (PrimRefAllocRef,   "allocRef#")
        , (PrimRefReadRef,    "readRef#")
        , (PrimRefWriteRef,   "writeRef#") ]


-- | Take the type of a primitive arithmetic operator.
typePrimRef :: PrimRef -> Type Name
typePrimRef op
 = case op of
        PrimRefAllocRef  
         -> tForalls [kRegion, kData] 
          $ \[tR, tA] -> tA 
                        `tFun` tSusp (tAlloc tR) (tRef tR tA)

        PrimRefReadRef   
         -> tForalls [kRegion, kData]
          $ \[tR, tA] -> tRef tR tA
                        `tFun` tSusp (tRead tR) tA

        PrimRefWriteRef  
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tRef tR tA `tFun` tA
                        `tFun` tSusp (tWrite tR) tUnit
