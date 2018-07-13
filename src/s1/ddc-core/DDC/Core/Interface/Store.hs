
module DDC.Core.Interface.Store
        ( -- * Types
          Store     (..)
        , Meta      (..)
        , Interface (..)
        , Error (..)

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
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName)
where
import DDC.Core.Interface.Store.Base
import DDC.Core.Interface.Store.Construct
import DDC.Core.Interface.Store.Fetch
import DDC.Core.Interface.Store.Resolve

