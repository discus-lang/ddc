
module DDC.Core.Interface
        ( Store
        , Interface     (..)
        , Meta          (..)
        , Error         (..)

        , new
        , addInterface
        , ensureInterface
        , fetchTransitiveImports
        , getMeta
        , getModuleNames
        , getInterfaces
        , findImportValue
        , importValuesOfStore)
where
import DDC.Core.Interface.Store
import DDC.Core.Interface.Error
import DDC.Core.Interface.Base
