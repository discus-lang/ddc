
module DDC.Core.Interface
        ( Interface     (..)
        , Store         (..)
        , Meta          (..)
        , Super         (..)
        , Error         (..)

        , new, wrap, load
        , importValuesOfStore
        , getMeta
        , getModuleNames
        , getInterfaces
        , findSuper)
where
import DDC.Core.Interface.Store
import DDC.Core.Interface.Error
import DDC.Core.Interface.Base
