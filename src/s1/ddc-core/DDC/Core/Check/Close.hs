
module DDC.Core.Check.Close
        ( closeModuleWithOracle
        , closeImportThings)
where
import DDC.Core.Module
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Interface.Resolve               (TyConThing(..))
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Interface.Resolve     as Store
import qualified DDC.Core.Interface.Base        as Store
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Collect.FreeT         as FreeT
import Data.Map                                 (Map)
import Data.IORef
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set

---------------------------------------------------------------------------------------------------
-- | Use the cache in the given interface oracle to add import declarations
--   to the module that cover all the free variable. The resulting module
--   will have no 'import module' specifications, and no need for them.
closeModuleWithOracle
        :: (Ord n, Show n, Show a)
        => KindEnv n
        -> Oracle.Oracle n
        -> Module (AnTEC a n) n
        -> CheckM a n (Module (AnTEC a n) n)

closeModuleWithOracle kenv oracle mm
 = do
        dataTypesByTyCon   <- liftIO $ readIORef (Oracle.oracleCacheDataTypesByTyCon oracle)
        typeSynsByTyCon    <- liftIO $ readIORef (Oracle.oracleCacheTypeSynsByTyCon  oracle)

        -- TODO: add this
        _dataCtorsByDaCon  <- liftIO $ readIORef (Oracle.oracleCacheDataCtorsByDaCon oracle)

        valuesByName       <- liftIO $ readIORef (Oracle.oracleCacheValuesByName     oracle)
        foreignTypesByName <- liftIO $ readIORef (Oracle.oracleCacheForeignTypesByTyCon oracle)

        its
         <- liftIO
          $ closeImportThings kenv (Oracle.oracleStore oracle)
          $ Map.unions
          [ Map.fromList [ (n, ImportThingDataType n dt)
                         | (n, dt)  <- Map.toList dataTypesByTyCon ]

          , Map.fromList [ (n, ImportThingSyn n k t)
                         | (n, (k, t)) <- Map.toList typeSynsByTyCon ]

          , Map.fromList [ (n, ImportThingValue n iv)
                         | (n, iv) <- Map.toList valuesByName ]

          , Map.fromList [ (n, ImportThingForeign n it)
                         | (n, it) <- Map.toList foreignTypesByName ] ]

        return $ mm
         { moduleImportModules
                = []

         , moduleImportDataDefs
                =  moduleImportDataDefs mm
                ++ [ (n, dataDefOfDataType dt)
                        | (n, ImportThingDataType _ dt) <- Map.toList its ]

         , moduleImportTypeDefs
                =  moduleImportTypeDefs mm
                ++ [ (n, (k, t))
                        | (n, ImportThingSyn _  k t)    <- Map.toList its ]

         , moduleImportTypes
                =  moduleImportTypes mm
                ++ [ (n, it)
                        | (n, ImportThingForeign _ it)  <- Map.toList its ]

         , moduleImportValues
                =  moduleImportValues   mm
                ++ [ (n, iv)
                        | (n, ImportThingValue _ iv)    <- Map.toList its ]
         }


---------------------------------------------------------------------------------------------------
-- | Things that can be imported into a module.
data ImportThing n
        = ImportThingDataType   n (DataType n)
        | ImportThingDataCtor   n (DataCtor n)
        | ImportThingForeign    n (ImportType n  (Kind n))
        | ImportThingSyn        n (Kind n) (Type n)
        | ImportThingValue      n (ImportValue n (Type n))
        deriving Show


-- | Take the name of an `ImportThing`.
nameOfImportThing :: ImportThing n -> n
nameOfImportThing it
 = case it of
        ImportThingDataType n _ -> n
        ImportThingDataCtor n _ -> n
        ImportThingForeign n _  -> n
        ImportThingSyn n _ _    -> n
        ImportThingValue n _    -> n


-- | Convert a `TyConThing` to an `ImportThing`
importOfTyConThing :: TyConThing n -> Maybe (ImportThing n)
importOfTyConThing tt
 = case tt of
        TyConThingPrim _ _      -> Nothing
        TyConThingData n dt     -> Just $ ImportThingDataType n dt
        TyConThingForeign n it  -> Just $ ImportThingForeign n it
        TyConThingSyn n k t     -> Just $ ImportThingSyn n k t


closeImportThings
        :: (Ord n, Show n)
        => KindEnv n
        -> Store.Store n
        -> Map n (ImportThing n)
        -> IO (Map n (ImportThing n))

closeImportThings kenv store mpThings
 = go   (Set.fromList $ Map.keys mpThings)
        Map.empty
        mpThings
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
         -- We don't need imporst for primitive things.
         | Just _ <- Env.lookupName n kenv
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

        ImportThingForeign{}
         -> Set.empty

        ImportThingSyn _ _ t
         -> supportOfType kenv t

        ImportThingValue _ iv
         -> supportOfType kenv $ typeOfImportValue iv


-- | Names of type constructors used in the given type.
supportOfType :: Ord n => KindEnv n -> Type n -> Set n
supportOfType kenv tt
 = let  (_, bs) = FreeT.freeVarConT kenv tt
        ns      = [ n | UName n <- Set.toList bs ]
   in   Set.fromList ns


