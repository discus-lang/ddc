
-- | Defines the Salt fragment of Disciple Core.
--
--   This fragment includes just those features that can be easily mapped onto
--   C or LLVM code. It has functions, case expressions and primops, but no 
--   partial application, data types, or nested functions etc. All operations
--   on algebraic data need to have been expanded to raw memory operations.
--
module DDC.Core.Salt.Output
        ( -- * Language profile
          profile

          -- * Conversion to C source code
        , convertModule
        , Error(..)

          -- * Names of variables and constructors
        , Name          (..)
        , module DDC.Core.Salt.Base
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
import DDC.Core.Salt.Base
import DDC.Core.Salt.Output.Name
import DDC.Core.Salt.Output.Profile
import DDC.Core.Salt.Output.Convert
import DDC.Core.Salt.Output.Error
