
-- | Disciple Core Salt.
--
--   This is what happens to 'C' when you leave it out in the sun for too long.
--
--   Salt is a fragment of System-F2 that contains just those features that 
--   can be easily mapped onto C or LLVM code. It has functions, case
--   expressions and primops, but no partial application, data types, or nested
--   functions. All operations on algebraic data need to have been expanded to
--   primitive store operations.
-- 
--   Salt exposes raw store and control primops, so its possible for functions
--   written directly in Salt to corrupt the heap (if they are wrong).
--
module DDC.Core.Salt
        ( -- * Language profile
          profile

          -- * Conversion
        , seaOfSaltModule
        , Error(..)

          -- * Names
        , Name          (..)
        , PrimTyCon     (..)
        , PrimOp        (..)
        , PrimCast      (..)
        , PrimCall      (..)
        , PrimControl   (..)
        , PrimStore     (..)
        , PrimArith     (..)

          -- * Name parsing
        , readName

          -- * Name lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Salt.Name
import DDC.Core.Salt.Profile
import DDC.Core.Salt.Convert
