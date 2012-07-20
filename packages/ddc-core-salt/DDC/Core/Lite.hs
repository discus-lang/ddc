
-- | The Lite fragment of Disciple Core.
--
--   This fragment has the polymorphism of System-F, along with real data types.
--
--   It does not support user-defined data types, yet, but has Units, Ints, Pairs and Lists baked in. 
--
module DDC.Core.Lite
        ( -- * Language profile
          profile

          -- * Conversion to the Disciple Core Salt.
        , toSalt
        , Error         (..)

          -- * Names of variables and constructors.
        , Name          (..)
        , DataTyCon     (..)
        , PrimTyCon     (..)
        , PrimDaCon     (..)
        , PrimOp        (..)
        , readName

          -- * Lexers
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Lite.Name
import DDC.Core.Lite.Profile
import DDC.Core.Lite.Convert
import DDC.Core.Salt.Name.Prim
