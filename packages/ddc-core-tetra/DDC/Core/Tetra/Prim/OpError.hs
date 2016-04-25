
module DDC.Core.Tetra.Prim.OpError
        ( OpError (..)
        , readOpError
        , typeOpError)
where
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpError where
 rnf op
  = case op of
        OpErrorDefault          -> ()


instance Pretty OpError where
 ppr op
  = case op of
        OpErrorDefault          -> text "default#"


-- | Read a primitive error operator.
readOpError :: String -> Maybe OpError
readOpError str
 = case str of
        "default#"      -> Just $ OpErrorDefault
        _               -> Nothing


-- | Get the type of a primitive error operator.
typeOpError :: OpError -> Type Name
typeOpError err
 = case err of
        OpErrorDefault    
         -> tForall kData $ \t -> tTextLit `tFun` tNat `tFun` t
