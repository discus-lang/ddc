
module DDC.Core.Tetra.Prim.OpCast
        ( readPrimCastFlag
        , typePrimCastFlag)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Exp.Simple
import Data.List


-- | Take the type of a primitive numeric cast operator.
typePrimCastFlag :: PrimCast -> Bool -> Type Name
typePrimCastFlag op bUnboxed
 = let  fb | bUnboxed   = tUnboxed
           | otherwise  = id

   in case op of
        PrimCastConvert  
         -> tForalls [kData, kData] $ \[t1, t2] -> fb t1 `tFun` fb t2

        PrimCastPromote  
         -> tForalls [kData, kData] $ \[t1, t2] -> fb t1 `tFun` fb t2

        PrimCastTruncate 
         -> tForalls [kData, kData] $ \[t1, t2] -> fb t1 `tFun` fb t2


-- | Read a primitive cast operator.
readPrimCastFlag :: String -> Maybe (PrimCast, Bool)
readPrimCastFlag str
  =  case find (\(_, n) -> str == n) primCastNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
primCastNames :: [((PrimCast, Bool), String)]
primCastNames
 = concat 
        $ [ [ ((p, False),  str)
            , ((p, True),   str ++ "#")]  
          | (p, str) <- table]
 where
  table 
   =    [ (PrimCastPromote,     "promote#")
        , (PrimCastTruncate,    "truncate#")
        , (PrimCastConvert,     "convert#") ]

