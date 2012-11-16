
-- | Disciple Core Lite.
--
--   This is a desugared version of Disciple Core that has all the polymorphism
--   of System-F2 along with algebraic data types. It does not yet support
--   user-defined data types, but has Units, Ints, Pairs and Lists baked in.
--
--   Lite exposes arithmetic primops like @add#@ and @or#@, but no store or
--   control primops. Code written in Lite cannot corrupt the heap, assuming
--   the implementation of the Salt primitives it uses (and compiler) is
--   correct.
--
module DDC.Core.Lite
        ( -- * Language profile
          profile

          -- * Conversion
        , saltOfLiteModule
        , Error         (..)

          -- * Names
        , Name          (..)
        , DataTyCon     (..)
        , PrimTyCon     (..)
        , PrimDaCon     (..)
        , PrimArith     (..)
        , PrimCast      (..)

          -- * Name Parsing
        , readName

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Lite.Name
import DDC.Core.Lite.Profile
import DDC.Core.Lite.Convert
