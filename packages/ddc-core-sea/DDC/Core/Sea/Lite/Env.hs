
module DDC.Core.Sea.Lite.Env
        ( -- * Kinds
          primKindEnv
        , kindOfName 

          -- * Types
        , primTypeEnv)
where
import DDC.Core.Sea.Lite.Name
import DDC.Core.Sea.Base.Name
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfName Env.empty


-- | Take the kind of a primitive name.
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConPtr     -> kData `kFun` kData
        PrimTyConAddr    -> kData
        PrimTyConNat     -> kData
        PrimTyConTag     -> kData
        PrimTyConBool    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConInt   _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConString  -> kData


kindOfName :: Name -> Maybe (Kind Name)
kindOfName nn
 = case nn of
        NamePrimTyCon tc  -> Just $ kindOfPrimTyCon tc
        _                 -> Nothing


-- Types ----------------------------------------------------------------------
primTypeEnv :: Env Name
primTypeEnv = Env.empty
