
-- | Name resolution.
module DDC.Core.Check.Context.Resolve
        ( TyConThing(..)
        , resolveTyConThing
        , lookupTypeSyn
        , lookupDataType
        , lookupDataCtor
        , lookupTypeOfValueName)
where
import DDC.Core.Check.State
import DDC.Core.Check.Context.Base
import DDC.Core.Check.Context.Oracle    (TyConThing(..))
import DDC.Core.Check.Error
import DDC.Type.Exp
import DDC.Type.DataDef
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified Data.Map.Strict        as Map



-------------------------------------------------------------------------------
-- | Resolve the name of a data type, type synonym, for foreign type.
--
--   These are all named after a type constructor, but they have different
--   sorts of declarations.
--
--   If we can't find one then throw an error in the `CheckM` monad.
--
resolveTyConThing
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (TyConThing n, Kind n)

resolveTyConThing ctx n
 = lookupTyConThing ctx n
 >>= \case
        Nothing -> throw  $ ErrorType $ ErrorTypeUndefinedTypeCtor (UName n)
        Just tk -> return tk


-- | Lookup the name of a data type, type synonym, for foreign type.
--
--   If we can't find it then `Nothing`.
--
lookupTyConThing
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (TyConThing n, Kind n))

lookupTyConThing ctx n
 -- Look for a primitive type of the same name.
 | Just k        <- EnvT.envtPrimFun (contextEnvT ctx) n
 = return $ Just (TyConThingPrim n k, k)

 -- Look for a data type defined in the current module.
 | dataDefs      <- EnvX.envxDataDefs $ contextEnvX ctx
 , Just dataType <- Map.lookup n (dataDefsTypes dataDefs)
 = return $ Just ( TyConThingData n dataType
                 , kindOfDataType dataType)

 -- Look for a type synonym defined in the current module.
 | Just tBind    <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx
 , Just kBind    <- Map.lookup n $ EnvT.envtMap $ contextEnvT ctx
 = return $ Just  ( TyConThingSyn n kBind tBind
                  , kBind)

 -- Look for a foreign type defined in the current module.
 | Just it      <- Map.lookup n $ EnvT.envtForeignTypes $ contextEnvT ctx
 = return $ Just  ( TyConThingForeign n it
                  , C.kindOfImportType it )

 -- Look for a data type, synonym or foreign type in an imported module.
 | Just oracle  <- contextOracle ctx
 = Oracle.resolveTyConThing oracle n
 >>= \case
        Nothing    -> return Nothing
        Just thing -> return $ Just (thing, Oracle.kindOfTyConThing thing)

 -- It's just not there.
 | otherwise
 = return Nothing


-------------------------------------------------------------------------------
-- | Lookup the definition of a type synonym from its name.
--
--   If we can't find it then `Nothing`.
--
lookupTypeSyn
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (Type n))

lookupTypeSyn ctx n
 -- Look for synonyom in the current module.
 | Just tR  <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx
 = return $ Just tR

 -- Look for synonym in imported modules.
 | Just oracle <- contextOracle ctx
 = Oracle.resolveTyConThing oracle n
 >>= \case
        Just (TyConThingSyn _ _ t) -> return $ Just t
        Just _  -> return Nothing
        Nothing -> return Nothing

 -- It's just not there.
 | otherwise    = return Nothing


-------------------------------------------------------------------------------
-- | Lookup the definition of a data type from its constructor name.
lookupDataType
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (DataType n))

lookupDataType ctx n
 -- Look for data type definition in the current module.
 | dataDefs      <- EnvX.envxDataDefs $ contextEnvX ctx
 , Just dataType <- Map.lookup n (dataDefsTypes dataDefs)
 = return $ Just dataType

 -- Look for data type definition in an imported module.
 | Just oracle <- contextOracle ctx
 = Oracle.resolveTyConThing oracle n
 >>= \case
        Nothing   -> return Nothing
        Just (TyConThingData _ dataType) -> return $ Just dataType
        Just _    -> return Nothing

 -- It's just not there.
 | otherwise    = return Nothing


-------------------------------------------------------------------------------
-- | Lookup the definition of a data constructor.
--   If we can't find it then `Nothing`.
lookupDataCtor
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (DataCtor n))

lookupDataCtor ctx n

 -- Look for data ctor in the current module, or as a primitive.
 | dataDefs      <- EnvX.envxDataDefs $ contextEnvX ctx
 , Just dataCtor <- Map.lookup n (dataDefsCtors dataDefs)
 = return $ Just dataCtor

 -- Look for data ctor in imported modules.
 | Just oracle <- contextOracle ctx
 = Oracle.resolveDataCtor oracle n

 -- It's just not there.
 | otherwise    = return Nothing


-------------------------------------------------------------------------------
-- | Lookup the type of a value.
--   If we can't find then `Nothing`.
lookupTypeOfValueName
        :: (Ord n, Show n)
        => Context n -> n -> CheckM a n (Maybe (Type n))

lookupTypeOfValueName ctx n
 -- Look for value on the context stack.
 | Just t       <- lookupType (UName n) ctx
 = return $ Just t

 -- Look for value at the top level of the current module,
 -- or as a primitive.
 | Just t       <- EnvX.lookupX (UName n) $ contextEnvX ctx
 = return $ Just t

 -- Look for value in imported modules.
 | Just oracle  <- contextOracle ctx
 = Oracle.resolveValueName oracle n
 >>= \case
        Just ivs  -> return $ Just $ C.typeOfImportValue ivs
        Nothing   -> return Nothing

 -- Its just not there.
 | otherwise    = return Nothing


