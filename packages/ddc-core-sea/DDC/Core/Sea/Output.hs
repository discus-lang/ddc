
-- | Defines the Sea Output fragment of Disciple Core.
--
--   This can be converted directly to C or LLVM code.
module DDC.Core.Sea.Output
        ( -- * Language profile
          profile

          -- * Conversion to C source code
        , convertModule
        , Error(..)

          -- * Names of variables and constructors
        , Name          (..)
        , module DDC.Core.Sea.Base
        , Prim          (..)
        , PrimCast      (..)
        , PrimCall      (..)
        , PrimControl   (..)
        , PrimStore     (..)
        , PrimExternal  (..)
        , readName
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Sea.Base
import DDC.Core.Sea.Output.Name
import DDC.Core.Sea.Output.Profile
import DDC.Core.Sea.Output.Convert
import DDC.Core.Sea.Output.Error
