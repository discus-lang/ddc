
module DDC.Core.Interface.Resolve
        ( TyConThing (..)
        , Error(..)
        , kindOfTyConThing
        , resolveTyConThing)
where
import DDC.Core.Interface.Store
import DDC.Core.Module.Name
import DDC.Type.DataDef
import DDC.Type.Exp
import Data.IORef
import Data.Maybe
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map


-- | Things named after type constructors.
data TyConThing n
        = TyConThingData n (DataType n)
        | TyConThingSyn  n (Kind n) (Type n)
        deriving Show

kindOfTyConThing :: TyConThing n -> Kind n
kindOfTyConThing thing
 = case thing of
        TyConThingData _ def    -> kindOfDataType def
        TyConThingSyn _ k _     -> k


-- | Things that can go wrong during name resolution.
data Error n
        = ErrorNotFound n
        | ErrorMultipleModules    (Set ModuleName)
        | ErrorMultipleTyConThing [TyConThing n]

        -- | The index maps in the interface store are broken.
        --   They said the store contained some value but it didn't.
        | ErrorInternal           String
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a type constructor or type synonym.
resolveTyConThing
        :: (Ord n, Show n)
        => Store n         -- ^ Interface store.
        -> Set ModuleName  -- ^ Modules whose exports we will search for the type thing.
        -> n               -- ^ Name of desired type thing.
        -> IO (Either (Error n) (TyConThing n))

resolveTyConThing store mnsImported n
 = goModules
 where
        -- Use the store index to determine which modules visibly expose a
        -- type thing with the desired name. If multiple modules do then
        -- the reference is ambiguous, and we produce an error.
        goModules
         = do   mnsWithTyCon    <- readIORef $ storeTypeCtorNames store
                let mnsAvail    = fromMaybe Set.empty $ Map.lookup n mnsWithTyCon
                let mnsVisible  = Set.intersection mnsAvail mnsImported
                case Set.toList mnsVisible of
                 []             -> return $ Left $ ErrorNotFound n
                 (_ : _ : _)    -> return $ Left $ ErrorMultipleModules mnsVisible
                 [mn]           -> goDetermine mn

        -- We know that a type thing of the required name is in the given module,
        -- so find out if it's a data type or synonym name.
        goDetermine mn
         = do   mDataDef        <- goGetDataTypes mn
                mTypeDef        <- goGetTypeDefs  mn
                case catMaybes $ concat $ sequence [ mDataDef, mTypeDef ] of
                 []             -> return $ Left $ ErrorInternal "broken store index"
                 ts@(_ : _ : _) -> return $ Left $ ErrorMultipleTyConThing ts
                 [thing]        -> return $ Right thing

        -- Look for a data type declaration in the given module.
        goGetDataTypes mn
         = do   dataTypesByTyCon        <- readIORef $ storeDataTypesByTyCon store
                case Map.lookup mn dataTypesByTyCon of
                 Nothing                -> return $ Left $ ErrorInternal "broken store index"
                 Just dataTypes
                  -> case Map.lookup n dataTypes of
                        Nothing         -> return $ Right Nothing
                        Just dataType   -> return $ Right $ Just $ TyConThingData n dataType

        -- Look for a type synonym declaration in the given module.
        goGetTypeDefs mn
         = do   typeDefsByTyCon <- readIORef $ storeTypeDefsByTyCon store
                case Map.lookup mn typeDefsByTyCon of
                 Nothing        -> return $ Left $ ErrorInternal "broken store index"
                 Just typeDefs
                  -> case Map.lookup n typeDefs of
                        Nothing         -> return $ Right Nothing
                        Just (k, t)     -> return $ Right $ Just $ TyConThingSyn n k t

