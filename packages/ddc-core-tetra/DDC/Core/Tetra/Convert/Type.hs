
module DDC.Core.Tetra.Convert.Type
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
import DDC.Core.Tetra.Convert.Type.Kind
import DDC.Core.Tetra.Convert.Type.Region
import DDC.Core.Tetra.Convert.Type.Witness
import DDC.Core.Tetra.Convert.Type.Super
import DDC.Core.Tetra.Convert.Type.DaCon
import DDC.Core.Tetra.Convert.Type.Data
import DDC.Core.Tetra.Convert.Type.Base
