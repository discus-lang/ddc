
module DDC.Core.Sea.Output
        ( -- * Language profile
          profile
        , lexString

          -- * Names of variables and constructors
        , Name          (..)
        , readName

          -- * Conversion to C source code
        , convertModule
        , Error(..))
where
import DDC.Core.Sea.Output.Name
import DDC.Core.Sea.Output.Profile
import DDC.Core.Sea.Output.Convert
