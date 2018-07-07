
module DDC.Core.Interface.Resolve
        ( TyConThing (..)
        , Error(..)
        , kindOfTyConThing
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName)
where
import DDC.Core.Interface.Store
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import Data.IORef
import Data.Maybe
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map


---------------------------------------------------------------------------------------------------
-- | Things named after type constructors.
data TyConThing n
        = TyConThingPrim    n (Kind n)
        | TyConThingData    n (DataType n)
        | TyConThingForeign n (ImportType n (Kind n))
        | TyConThingSyn     n (Kind n) (Type n)
        deriving Show

kindOfTyConThing :: TyConThing n -> Kind n
kindOfTyConThing thing
 = case thing of
        TyConThingPrim    _ k   -> k
        TyConThingData    _ def -> kindOfDataType def
        TyConThingSyn     _ k _ -> k
        TyConThingForeign _ def -> kindOfImportType def


---------------------------------------------------------------------------------------------------
-- | Things that can go wrong during name resolution.
data Error n
        = ErrorNotFound n
        | ErrorMultipleModules    n (Set ModuleName)
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
         = do   -- Set of all modules that export a tycon thing with the desired name,
                -- independent of whether that module is imported into the current one.
                mnsWithTyCon    <- readIORef $ storeTypeCtorNames store
                let mnsAvail    = fromMaybe Set.empty $ Map.lookup n mnsWithTyCon

                -- Set of all modules that export a tycon thing with the desired name,
                -- and are visibly imported into the current one.
                let mnsVisible  = Set.intersection mnsAvail mnsImported

                -- We know that type things with the given names are visible via these
                -- modules, so go find out what they are.
                dataDefs       <- goGetDataTypes   mnsVisible
                typeDefs       <- goGetTypeDefs    mnsVisible
                foreignDefs    <- goGetForeignType mnsVisible

                let things      = concat [ dataDefs, typeDefs, foreignDefs ]

                case things of
                 []             -> return $ Left $ ErrorNotFound n
                 ts@(_ : _ : _) -> return $ Left $ ErrorMultipleTyConThing ts
                 [thing]        -> return $ Right thing

        -- Look for a data type declarations in the given modules.
        goGetDataTypes mns
         = do   dataTypesByTyCon
                 <- readIORef $ storeDataTypesByTyCon store

                return  $ map (TyConThingData n)
                        $ mapMaybe (\mn -> Map.lookup mn dataTypesByTyCon >>= Map.lookup n)
                        $ Set.toList mns

        -- Look for a foreign type declaration in the given module.
        goGetForeignType mns
         = do   foreignTypesByTyCon
                 <- readIORef $ storeForeignTypesByTyCon store

                return  $ map (TyConThingForeign n)
                        $ mapMaybe (\mn -> Map.lookup mn foreignTypesByTyCon >>= Map.lookup n)
                        $ Set.toList mns


        -- Look for a type synonym declaration in the given module.
        goGetTypeDefs mns
         = do   typeSynsByTyCon
                 <- readIORef $ storeTypeSynsByTyCon store

                return  $ map (\(k, t) -> TyConThingSyn n k t)
                        $ squashTypeSyns
                        $ mapMaybe (\mn -> Map.lookup mn typeSynsByTyCon >>= Map.lookup n)
                        $ Set.toList mns

        -- Eliminate duplicate synonym decls from the given list.
        -- TODO: we should really tag these with the original definition module,
        --       to ensure multiple of the same type and name are not defined
        --       in separate modules.
        squashTypeSyns kts
         = case kts of
                []              -> []
                ((k, t) : moar) -> (k, t) : filter (\(k', t') -> k /= k' && t /= t') moar



---------------------------------------------------------------------------------------------------
-- | Resolve the name of a data constructor.
resolveDataCtor
        :: (Ord n, Show n)
        => Store n         -- ^ Interface store.
        -> Set ModuleName  -- ^ Modules whose exports we will search for the ctor.
        -> n               -- ^ Name of desired ctor.
        -> IO (Either (Error n) (DataCtor n))

resolveDataCtor store mnsImported n
 = goModules
 where
        -- Use the store index to determine which modules visibly expose a
        -- data ctor with the desired name. If multiple modules do then
        -- the reference is ambiguous, and we produce an error.
        goModules
         = do   mnsWithDaCon    <- readIORef $ storeDataCtorNames store
                let mnsAvail    = fromMaybe Set.empty $ Map.lookup n mnsWithDaCon
                let mnsVisible  = Set.intersection mnsAvail mnsImported
                case Set.toList mnsVisible of
                 []             -> return $ Left $ ErrorNotFound n
                 (_ : _ : _)    -> return $ Left $ ErrorMultipleModules n mnsVisible
                 [mn]           -> goGetDataCtor mn

        goGetDataCtor mn
         = do   dataCtorsByDaCon <- readIORef $ storeDataCtorsByDaCon store
                case Map.lookup mn dataCtorsByDaCon of
                 Nothing        -> return $ Left $ ErrorInternal "broken store index"
                 Just dataCtors
                  -> case Map.lookup n dataCtors of
                        Nothing         -> return $ Left $ ErrorNotFound n
                        Just dataCtor   -> return $ Right $ dataCtor


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a value binding.
--
--   This returns the `ImportValue` that could be added to a module
--   to import the value into scope.
resolveValueName
        :: (Ord n, Show n)
        => Store n          -- ^ Interface store.
        -> Set ModuleName   -- ^ Modules whose exports we will search for the name.
        -> n                -- ^ Name of desired value.
        -> IO (Either (Error n) (ImportValue n (Type n)))

resolveValueName store mnsImported n
 = goModules
 where
        -- Use the store index to determine which modules visibly expose a
        -- data ctor with the desired name. If multiple modules do then
        -- the reference is ambiguous, and we produce an error.
        goModules
         = do   mnsWithValue    <- readIORef $ storeValueNames store
                let mnsAvail    = fromMaybe Set.empty $ Map.lookup n mnsWithValue
                let mnsVisible  = Set.intersection mnsAvail mnsImported
                case Set.toList mnsVisible of
                 []             -> return $ Left $ ErrorNotFound n
                 (_ : _ : _)    -> return $ Left $ ErrorMultipleModules n mnsVisible
                 [mn]           -> goGetImportValue mn


        goGetImportValue mn
         = do   valuesByName    <- readIORef $ storeValuesByName store
                case Map.lookup mn valuesByName of
                 Nothing        -> return $ Left $ ErrorInternal "broken store index"
                 Just importValues
                  -> case Map.lookup n importValues of
                        Nothing          -> return $ Left $ ErrorNotFound n
                        Just importValue -> return $ Right $ importValue

