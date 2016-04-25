
module DDC.Core.Tetra.Prim.OpError
        ( OpError (..)
        , readOpErrorFlag
        , typeOpErrorFlag)
where
import DDC.Core.Tetra.Prim.TyConTetra
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
        OpErrorDefault  -> text "default#"


-- | Read a primitive error operator.
readOpErrorFlag :: String -> Maybe (OpError, Bool)
readOpErrorFlag str
 = case str of
        "default#"      -> Just (OpErrorDefault, False)
        "default##"     -> Just (OpErrorDefault, True)
        _               -> Nothing


-- | Get the type of a primitive error operator.
typeOpErrorFlag :: OpError -> Bool -> Type Name
typeOpErrorFlag err False
 = case err of
        OpErrorDefault    
         -> tForall kData $ \t -> tTextLit `tFun` tNat `tFun` t

typeOpErrorFlag err True
 = case err of
        OpErrorDefault    
         -> tForall kData $ \t -> tUnboxed tTextLit `tFun` tUnboxed tNat `tFun` t
