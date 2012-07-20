
-- | Defines the Salt fragment of Disciple Core.
--
--   This fragment includes just those features that can be easily mapped onto
--   C or LLVM code. It has functions, case expressions and primops, but no 
--   partial application, data types, or nested functions etc. All operations
--   on algebraic data need to have been expanded to raw memory operations.
--
module DDC.Core.Salt
        ( -- * Language profile
          profile

          -- * Conversion to C source code
        , convertModule
        , Error(..)

          -- * Names of variables and constructors
        , Name          (..)
        , PrimTyCon     (..)
        , Prim          (..)
        , PrimCast      (..)
        , PrimCall      (..)
        , PrimControl   (..)
        , PrimStore     (..)
        , PrimExternal  (..)
        , PrimOp        (..)
        , readName

          -- * Lexers
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Salt.Name
import DDC.Core.Salt.Profile
import DDC.Core.Salt.Convert
