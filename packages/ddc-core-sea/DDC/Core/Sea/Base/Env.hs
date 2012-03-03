
module DDC.Core.Sea.Base.Env
        ( primKindEnv
        , kindOfPrimTyCon
        , primTypeEnv)
where
import DDC.Core.Sea.Base.Name
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfName Env.empty


kindOfName :: Name -> Maybe (Kind Name)
kindOfName nn
 = case nn of
        NamePrimTyCon tc  -> Just $ kindOfPrimTyCon tc
        _                 -> Nothing


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimTyCon :: PrimTyCon -> Kind Name
kindOfPrimTyCon tc
 = case tc of
        PrimTyConPtr     -> kData `kFun` kData
        PrimTyConAddr    -> kData
        PrimTyConNat     -> kData
        PrimTyConTag     -> kData
        PrimTyConBool    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConInt   _ -> kData
        PrimTyConFloat _ -> kData



-- Types ----------------------------------------------------------------------
primTypeEnv :: Env Name
primTypeEnv = Env.empty
