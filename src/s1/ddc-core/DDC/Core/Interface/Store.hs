
module DDC.Core.Interface.Store
        ( -- * Types
          Store      (..)
        , Meta       (..)
        , Interface  (..)
        , TyConThing (..)
        , Error (..)

        -- * Construction
        , new, addInterface

        -- * Fetching Data
        , getMeta
        , getModuleNames
        , lookupInterface
        , fetchInterface
        , fetchTransitiveImports

        -- * Name Resolution
        , kindOfTyConThing
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName
        , resolveValueByResultTyCon)
where
import DDC.Core.Interface.Store.Base
import DDC.Core.Interface.Store.Construct
import DDC.Core.Interface.Store.Fetch
import DDC.Core.Interface.Store.Resolve

