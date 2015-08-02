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

          -- * Names
        , Name          (..)
        , PrimTyCon     (..)

          -- ** Primitive Values
        , PrimVal       (..)
        , pattern NamePrimOp
        , pattern NamePrimLit

          -- ** Primitive Operators
        , PrimOp        (..)
        , PrimArith     (..)
        , PrimCall      (..)
        , PrimCast      (..)
        , PrimControl   (..)
        , PrimStore     (..)

        , primCastPromoteIsValid
        , primCastTruncateIsValid

          -- ** Primitive Literals
        , PrimLit       (..)
        , pattern NameLitVoid
        , pattern NameLitBool
        , pattern NameLitNat
        , pattern NameLitInt
        , pattern NameLitSize
        , pattern NameLitWord
        , pattern NameLitFloat
        , pattern NameLitTextLit
        , pattern NameLitTag

          -- * Name parsing
        , readName
        , takeNameVar

          -- * Program lexing
        , lexModuleString
        , lexExpString

          -- * Conversion
        , seaOfSaltModule
        , Error(..)

          -- * Name generation
        , freshT
        , freshX

          -- * Salt expressions
        , module DDC.Core.Salt.Exp)
where
import DDC.Core.Salt.Name
import DDC.Core.Salt.Profile
import DDC.Core.Salt.Convert
import DDC.Core.Salt.Exp
