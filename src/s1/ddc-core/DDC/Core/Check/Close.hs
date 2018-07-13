
-- TODO: make this its own tranform under Core.Transform
module DDC.Core.Check.Close
        ( closeModuleWithOracle
        , closeImportThings)
where
import DDC.Core.Module
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Exp
import DDC.Type.DataDef
import DDC.Type.Env                             (KindEnv)
import DDC.Core.Check.Context.Oracle            (Oracle)
import DDC.Core.Interface.Store                 (Store, TyConThing(..))
import Data.Map                                 (Map)
import Data.Set                                 (Set)
import Data.Maybe
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Interface.Store       as Store
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Collect.FreeT         as FreeT
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import Data.IORef


-- TODO: refactor this into two steps
--   1) wrap the cached things around the module.
--   2) close the new set of module decls.
--
--  Then we can use the second part to close elaborated modules that now
--  refer to extra bindings
--
--  Do a single close step after both checking and elaboration.
--  Don't close after checking then re-close after elaboration.


---------------------------------------------------------------------------------------------------
-- | Use the cache in the given interface oracle to add import declarations
--   to the module that cover all the free variable. The resulting module
--   will have no 'import module' specifications, and no need for them.
closeModuleWithOracle
        :: (Ord n, Show n, Show a)
        => KindEnv n
        -> Oracle n
        -> Module (AnTEC a n) n
        -> IO (Module (AnTEC a n) n)

closeModuleWithOracle kenv oracle mm
 = do   itsOracle       <- importThingsOfOracleCache oracle

        -- TODO: we're auto re-exporting all the decls in imported modules
        -- until we have proper module exports in the source syntax.
        let store = Oracle.oracleStore oracle
        let mns   = moduleImportModules mm
        dataTypesByTyCon    <- readIORef $ Store.storeDataTypesByTyCon store
        foreignTypesByTyCon <- readIORef $ Store.storeForeignTypesByTyCon store
        typeSynsByTyCon     <- readIORef $ Store.storeTypeSynsByTyCon store
        capsByName          <- readIORef $ Store.storeCapsByName store

        let dsReDataTypes    = Map.unions $ mapMaybe (\mn -> Map.lookup mn dataTypesByTyCon) mns
        let dsReForeignTypes = Map.unions $ mapMaybe (\mn -> Map.lookup mn foreignTypesByTyCon) mns
        let dsReTypeSyns     = Map.unions $ mapMaybe (\mn -> Map.lookup mn typeSynsByTyCon) mns
        let dsReCaps         = Map.unions $ mapMaybe (\mn -> Map.lookup mn capsByName) mns

        let mm_reexport
                = mm
                { moduleImportDataDefs
                        = moduleImportDataDefs mm
                        ++ (Map.toList $ Map.map dataDefOfDataType dsReDataTypes)

                , moduleImportTypes
                        = moduleImportTypes mm ++ Map.toList dsReForeignTypes

                , moduleImportTypeDefs
                        = moduleImportTypeDefs mm ++ Map.toList dsReTypeSyns

                , moduleImportCaps
                        = moduleImportCaps mm ++ Map.toList dsReCaps
                }

        let itsModule   =  importThingsOfModule mm_reexport

        -- Names of types defined in the current module.
        let nsModule    = Set.unions
                        [ Set.fromList $ map fst $ moduleLocalDataDefs mm
                        , Set.fromList $ map fst $ moduleLocalTypeDefs mm ]

        its'    <- closeImportThings kenv (Oracle.oracleStore oracle) nsModule
                $  Map.union itsOracle itsModule

        return  $ wrapModuleWithImportThings its' mm_reexport


---------------------------------------------------------------------------------------------------
closeImportThings
        :: (Ord n, Show n)
        => KindEnv n
        -> Store n
        -> Set n                        -- ^ Names of types defined in current module.
        -> Map n (ImportThing n)
        -> IO (Map n (ImportThing n))

closeImportThings kenv store nsLocal mpThings
 = go   (Set.fromList $ Map.keys mpThings)
        Map.empty mpThings
 where
        go nsHave mpDone mpToCheck
         = do   -- Names of types needed by the to-check things.
                let nsNeeded    = Set.unions
                                $ map (supportOfImportThing kenv)
                                $ Map.elems mpToCheck

                -- Names of types we need to import that we don't already have.
                let nsImport    = Set.difference nsNeeded nsHave

                if Set.null nsImport
                 -- We've already got a closed set of things.
                 then return (Map.union mpDone mpToCheck)

                 -- We need to chase down more imports.
                 else do
                        lsImport
                         <- fmap catMaybes $ mapM chase $  Set.toList nsImport

                        let mpImport
                                = Map.fromList
                                $ [ (nameOfImportThing it, it) | it <- lsImport ]

                        go (Set.union nsHave nsImport)
                           (Map.union mpDone mpToCheck)
                           mpImport

        -- Chase down imports for the given name.
        chase n
         -- We don't need imports for primitive things.
         | Just _ <- Env.lookupName n kenv
         = return Nothing

         -- Import mentions a type defined in the current module,
         -- which can happen for foreign imports for abstract boxed objects.
         | Set.member n nsLocal
         = return Nothing

         | otherwise
         = do   -- TODO: We don't yet propagate qualified names through the whole
                -- compiler, so don't know where this name really came from.
                --
                -- For this to work the module that the name is defined in must have
                -- already been loaded into the interface store.
                --
                -- Doing this ignores the import list in the moduel being closed.
                --
                typeCtorNames   <- readIORef $ Store.storeTypeCtorNames store
                case Map.lookup n typeCtorNames of
                 Nothing        -> error $ "closeImportThings: can't find name" ++ show n
                 Just mns       -> chaseFromModules mns n

        -- Chase down imports for the given name.
        chaseFromModules mns n
         = Store.resolveTyConThing store mns n
         >>= \case
                Left err         -> error  $ "closeImportThings: " ++ show err
                Right tyConThing -> return $ importOfTyConThing tyConThing


---------------------------------------------------------------------------------------------------
-- | Convert a `TyConThing` to an `ImportThing`
importOfTyConThing :: TyConThing n -> Maybe (ImportThing n)
importOfTyConThing tt
 = case tt of
        TyConThingPrim    _ _     -> Nothing
        TyConThingData    n dt    -> Just $ ImportThingDataType n dt
        TyConThingForeign n it    -> Just $ ImportThingType     n it
        TyConThingSyn     n k t   -> Just $ ImportThingSyn      n k t


---------------------------------------------------------------------------------------------------
-- | Slurp a map of import things from the oracle cache.
--
--   The map can be keyed by plain names instead of namespace qualified names
--   because we only store vars of caps/values and cons of types, which
--   don't conflict.
--
importThingsOfOracleCache :: Ord n => Oracle n -> IO (Map n (ImportThing n))
importThingsOfOracleCache oracle
 = do   dataTypesByTyCon   <- readIORef (Oracle.oracleCacheDataTypesByTyCon oracle)
        typeSynsByTyCon    <- readIORef (Oracle.oracleCacheTypeSynsByTyCon oracle)
        foreignTypesByName <- readIORef (Oracle.oracleCacheForeignTypesByTyCon oracle)
        capsByName         <- readIORef (Oracle.oracleCacheCapsByName oracle)
        valuesByName       <- readIORef (Oracle.oracleCacheValuesByName oracle)

        return  $ Map.unions
                [ Map.mapWithKey ImportThingDataType dataTypesByTyCon
                , Map.mapWithKey (\n (k, t) -> ImportThingSyn n k t) typeSynsByTyCon
                , Map.mapWithKey ImportThingType     foreignTypesByName
                , Map.mapWithKey ImportThingCap      capsByName
                , Map.mapWithKey ImportThingValue    valuesByName ]


---------------------------------------------------------------------------------------------------
-- | Compute the support set of an import thing.
supportOfImportThing :: Ord n => KindEnv n -> ImportThing n -> Set n
supportOfImportThing kenv it
 = case it of
        ImportThingDataType _ dt
         -> Set.unions
                $ map (supportOfType kenv)
                $ concatMap dataCtorFieldTypes
                $ fromMaybe [] (dataTypeCtors dt)

        ImportThingDataCtor _ ctor
         -> Set.unions
                $ map (supportOfType kenv)
                $ dataCtorFieldTypes ctor

        ImportThingType{}       -> Set.empty
        ImportThingSyn   _ _ t  -> supportOfType kenv t
        ImportThingCap   _ ic   -> supportOfType kenv $ typeOfImportCap ic
        ImportThingValue _ iv   -> supportOfType kenv $ typeOfImportValue iv


-- | Names of type constructors used in the given type.
supportOfType :: Ord n => KindEnv n -> Type n -> Set n
supportOfType kenv tt
 = let  (_, bs) = FreeT.freeVarConT kenv tt
        ns      = [ n | UName n <- Set.toList bs ]
   in   Set.fromList ns

