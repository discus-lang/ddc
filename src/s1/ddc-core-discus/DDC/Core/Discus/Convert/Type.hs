
module DDC.Core.Discus.Convert.Type
        ( -- * Names
          convertBindNameM

          -- * Kinds
        , convertK
        , convertTypeB
        , convertTypeU

          -- * Region types
        , convertRegionT
        , saltPrimeRegionOfDataType

          -- * Data constructors
        , convertCtorT
        , convertDaCon

          -- * Capabilities
        , convertCapabilityT
        , convertCapabilityB

          -- * Data
        , convertDataB
        , convertDataU
        , convertDataT
        , convertDataPrimitiveT

          -- * Supers
        , convertSuperConsT)
where
import DDC.Core.Discus.Convert.Type.Kind
import DDC.Core.Discus.Convert.Type.Region
import DDC.Core.Discus.Convert.Type.Witness
import DDC.Core.Discus.Convert.Type.Super
import DDC.Core.Discus.Convert.Type.DaCon
import DDC.Core.Discus.Convert.Type.Data
import DDC.Core.Discus.Convert.Type.Base
