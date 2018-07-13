
module DDC.Core.Interface.Store
        ( Store     (..)
        , Meta      (..)
        , Interface (..)

        -- * Construction
        , new, addInterface

        -- * Fetching Data
        , getMeta
        , getModuleNames
        , getInterfaces
        , lookupInterface
        , ensureInterface
        , fetchInterface
        , fetchTransitiveImports
        , findImportValue       -- TODO: convert fetch fn.
        , importValuesOfStore   -- TODO: ditch this
        , typeSynsOfStore       -- TODO: ditch this

        -- * Name Resolution
        , TyConThing (..)
        , kindOfTyConThing
        , Error (..)
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName)
where
import DDC.Core.Interface.Store.Base
import DDC.Core.Interface.Store.Construct
import DDC.Core.Interface.Store.Fetch
import DDC.Core.Interface.Store.Resolve

