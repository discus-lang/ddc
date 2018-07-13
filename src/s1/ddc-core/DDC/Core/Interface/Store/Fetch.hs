{-# OPTIONS_HADDOCK hide #-}

module DDC.Core.Interface.Store.Fetch where
import DDC.Core.Interface.Store.Base
import DDC.Core.Interface.Store.Construct
import DDC.Core.Module.Import
import DDC.Core.Module
import DDC.Type.Exp
import Data.IORef
import Data.Set                         (Set)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- | Get metadata of interfaces currently in the store.
getMeta :: Store n -> IO [Meta]
getMeta store
 = do   mm      <- readIORef (storeMeta store)
        return  $ Map.elems mm


-- | Get names of the modules currently in the store.
getModuleNames :: Store n -> IO [ModuleName]
getModuleNames store
 = do   metas   <- readIORef (storeMeta store)
        return  $ Map.keys metas


---------------------------------------------------------------------------------------------------
-- | Lookup a module interface from the store.
lookupInterface :: Store n -> ModuleName -> IO (Maybe (Interface n))
lookupInterface store nModule
 = do   ints    <- readIORef (storeInterfaces store)
        return  $ Map.lookup nModule ints


-- | Fetch an interface from the store, or load it if we don't already have it.
--
--   TODO: fix API to not duplicate code for ensureInterface.
--
fetchInterface
        :: (Ord n, Show n)
        => Store n -> ModuleName
        -> IO (Maybe (Interface n))

fetchInterface store nModule
 | Just load    <- storeLoadInterface store
 = let
        goCheck
         = do   iis     <- readIORef $ storeInterfaces store
                case Map.lookup nModule iis of
                 Just ii -> return (Just ii)
                 Nothing -> goLoad

        goLoad
         = do   result  <- load nModule
                case result of
                 Nothing  -> return Nothing
                 Just ii
                  -> do addInterface store ii
                        return (Just ii)
   in   goCheck

 | otherwise
 = return Nothing


-- | Try to find and load the interface file for the given module into the store,
--   or do nothing if we already have it.

--   If the interface file cannot be found then return False, otherwise True.
--   If the interface file exists but cannot be loaded then `error`.
--   If there is no load function defined then `error`.
--
--   FIXME: we need to check that the interface file is fresh relative
--   to any existing source files and dependent modules. When statting the dep
--   modules also make sure to avoid restatting the same module over and over.
--   The top level compile driver used to do this job.
--
ensureInterface
        :: (Ord n, Show n)
        => Store n -> ModuleName -> IO Bool

ensureInterface store nModule
 | Just load   <- storeLoadInterface store
 = let
        goCheckMeta
         = do   meta <- readIORef $ storeMeta store
                case Map.lookup nModule meta of
                 Just _  -> return True
                 Nothing -> goLoad

        goLoad
         = do   result <- load nModule
                case result of
                 Nothing -> return False
                 Just ii
                  -> do addInterface store ii
                        return True
   in   goCheckMeta

 | otherwise
 = return False


---------------------------------------------------------------------------------------------------
-- | Extract the set of of transitively imported modules from the interface store.
--   Doing this will force load of needed interface files.
--
--   FIXME: we particularly want to avoid loading the complete interface
--          files just to get the list of .o files we need to link with.
--
--   Store the list of transitive imports directly in each module,
--   so we don't need to load the complete graph of imports.
--
fetchTransitiveImports
        :: (Ord n, Show n)
        => Store n -> ModuleName -> IO (Set ModuleName)

fetchTransitiveImports store mn
 = loop Set.empty (Set.singleton mn)
 where
        loop mnsHave mnsNext
         | mnNext : _       <- Set.toList mnsNext
         = if Set.member mnNext mnsHave
            then loop mnsHave (Set.delete mnNext mnsNext)
            else do
                Just ii <- fetchInterface store mnNext

                let mnsMoar
                        = Set.fromList $ moduleImportModules $ interfaceModule ii

                let mnsNext'
                        = Set.difference
                                (Set.union mnsNext mnsMoar)
                                (Set.insert mnNext mnsHave)

                loop (Set.insert mnNext mnsHave) mnsNext'

         | otherwise
         = return mnsHave


-- Caps -------------------------------------------------------------------------------------------
{- FIXME: load the caps.
importCapsOfInterface
        :: Ord n => Interface
        -> Map ModuleName (Map n (ImportCap n (Type n)))
= let
        importOfExport
-}



---------------------------------------------------------------------------------------------------
-- | Get a list of types of all top-level supers in all modules in the store.
--
--   TODO: multiple conflicting names get smashed together,
--   this is used by the elaborate pass which needs to be redone to use
--   the store directly.
--
importValuesOfStore :: (Ord n, Show n) => Store n -> IO [(n, ImportValue n (Type n))]
importValuesOfStore store
 = do   mnns       <- readIORef $ storeValuesByName store
        let mns    =  Map.toList $ Map.unions $ Map.elems mnns
        return mns


typeSynsOfStore :: (Ord n, Show n) => Store n -> IO [(n, Type n)]
typeSynsOfStore store
 = do   mnns       <- readIORef $ storeTypeSynsByTyCon store
        let mns    =  Map.toList $ Map.map snd $ Map.unions $ Map.elems mnns
        return mns

