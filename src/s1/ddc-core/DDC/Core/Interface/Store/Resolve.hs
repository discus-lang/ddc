
-- | Name resolution via the interface store.
module DDC.Core.Interface.Store.Resolve where
import DDC.Core.Interface.Store.Base
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


-- | Produce the kind of a `TyConThing`.
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
        --   TODO: ditch this.
        | ErrorInternal           String
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a type constructor, type synonym or foreign type.
resolveTyConThing
        :: (Ord n, Show n)
        => Store n         -- ^ Interface store.
        -> Set ModuleName  -- ^ Modules whose exports we will search for the type thing.
        -> n               -- ^ Name of desired type thing.
        -> IO (Either (Error n) (TyConThing n))

resolveTyConThing store mnsImported n
 = goModules
 where
        ---------------------------------------------------
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
                typeSyns       <- goGetTypeSyns    mnsVisible
                foreignDefs    <- goGetForeignType mnsVisible
                let things      = concat [ dataDefs, typeSyns, foreignDefs ]

                case things of
                 -- There weren't any things with the given name.
                 []             -> return $ Left $ ErrorNotFound n

                 -- We found multiple distinct things with the same name.
                 ts@(_ : _ : _) -> return $ Left $ ErrorMultipleTyConThing ts

                 -- We found a single thing with the desired name.
                 -- This is the happy case.
                 [thing]        -> return $ Right thing

        ---------------------------------------------------
        -- Look for a data type declarations with the desired name in the given modules.
        goGetDataTypes mns
         = do   dataTypesByTyCon
                 <- readIORef $ storeDataTypesByTyCon store

                return  $ map (TyConThingData n)
                        $ squashDataTypes
                        $ mapMaybe (\mn -> Map.lookup mn dataTypesByTyCon >>= Map.lookup n)
                        $ Set.toList mns

        -- Eliminate duplicate data type decls from the list.
        -- The same decl may have been imported then re-exported via multiple modules.
        squashDataTypes dts
         = case dts of
                []        -> []
                dt : dts' -> dt : filter (not . isSameDataType dt) dts'

        -- Check if these are the same data type declaration, assuming if the
        -- module and type names are the same then the other info is also the same.
        isSameDataType dt1 dt2
         =  (dataTypeModuleName dt1 == dataTypeModuleName dt2)
         && (dataTypeName dt1       == dataTypeName dt2)

        ---------------------------------------------------
        -- Look for foreign type declarations with the desired name in the given modules.
        goGetForeignType mns
         = do   foreignTypesByTyCon
                 <- readIORef $ storeForeignTypesByTyCon store

                return  $ map (TyConThingForeign n)
                        $ squashForeignTypes
                        $ mapMaybe (\mn -> Map.lookup mn foreignTypesByTyCon >>= Map.lookup n)
                        $ Set.toList mns

        -- TODO: we should tag these with the original definition module,
        --       to ensure multiple of the same type and name are not defined
        --       in separate modules.
        -- The same decl may have been imported then re-exported via multiple modules.
        squashForeignTypes ivs
         = case ivs of
                []        -> []
                iv : ivs' -> iv : filter (not . isSameForeignType iv) ivs'

        isSameForeignType iv1 iv2
         = iv1 == iv2

        ---------------------------------------------------
        -- Look for a type synonym declarations with the desired name in the given modules.
        goGetTypeSyns mns
         = do   typeSynsByTyCon
                 <- readIORef $ storeTypeSynsByTyCon store

                return  $ map (\(k, t) -> TyConThingSyn n k t)
                        $ squashTypeSyns
                        $ mapMaybe (\mn -> Map.lookup mn typeSynsByTyCon >>= Map.lookup n)
                        $ Set.toList mns

        -- Eliminate duplicate synonym decls from the given list.
        -- The same decl may have been imported then re-exported via multiple modules.
        -- TODO: we should tag these with the original definition module,
        --       to ensure multiple of the same type and name are not defined
        --       in separate modules.
        squashTypeSyns kts
         = case kts of
                []        -> []
                kt : kts' -> kt : filter (not . isSameTypeSyn kt) kts'

        isSameTypeSyn kt1 kt2
         = kt1 == kt2


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a data constructor.
resolveDataCtor
        :: (Ord n, Show n)
        => Store n         -- ^ Interface store.
        -> Set ModuleName  -- ^ Modules whose exports we will search for the ctor.
        -> n               -- ^ Name of desired ctor.
        -> IO (Either (Error n) (DataCtor n, DataType n))

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
                ctors           <- goGetDataCtors mnsVisible
                case ctors of
                 []             -> return $ Left $ ErrorNotFound n
                 (_ : _ : _)    -> return $ Left $ ErrorMultipleModules n mnsVisible
                 [ctor]         -> return $ Right ctor

        goGetDataCtors mns
         = do   dataCtorsByDaCon
                 <- readIORef $ storeDataCtorsByDaCon store

                return  $ squashDataCtors
                        $ mapMaybe (\mn -> Map.lookup mn dataCtorsByDaCon >>= Map.lookup n)
                        $ Set.toList mns

        squashDataCtors ctors
         = case ctors of
                []        -> []
                ct : cts' -> ct : filter (not . isSameDataCtor ct) cts'

        isSameDataCtor (ct1, _dt1) (ct2, _dt2)
         =  (dataCtorModuleName ct1 == dataCtorModuleName ct2)
         && (dataCtorName ct1       == dataCtorName ct2)


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
                ivs             <- goGetImportValues mnsVisible
                case ivs of
                 []             -> return $ Left $ ErrorNotFound n
                 (_ : _ : _)    -> return $ Left $ ErrorMultipleModules n mnsVisible
                 [iv]           -> return $ Right iv


        goGetImportValues mns
         = do   valuesByName
                 <- readIORef $ storeValuesByName store

                return  $ squashImportValues
                        $ mapMaybe (\mn -> Map.lookup mn valuesByName >>= Map.lookup n)
                        $ Set.toList mns

        squashImportValues ivs
         = case ivs of
                []        -> []
                iv : ivs' -> iv : filter (not . isSameImportValue iv) ivs'

        isSameImportValue iv1 iv2
         =  (importValueModuleName iv1 == importValueModuleName iv2)
         && (importValueModuleVar  iv1 == importValueModuleVar  iv2)

