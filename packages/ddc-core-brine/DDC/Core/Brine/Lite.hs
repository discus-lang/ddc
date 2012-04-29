
-- | The Lite fragment of Disciple Core.
--
--   This fragment has the polymorphism of System-F, along with real data types.
--
--   It does not support user-defined data types, yet, but has Units, Ints, Pairs and Lists baked in. 
--
module DDC.Core.Brine.Lite
        ( -- * Language profile
          profile
        , lexModuleString
        , lexExpString

          -- * Conversion to the Disciple Core Brine.
        , toBrine
        , Error         (..)

          -- * Names of variables and constructors.
        , Name          (..)
        , DataTyCon     (..)
        , PrimDaCon     (..)
        , readName)
where
import DDC.Core.Brine.Lite.Profile
import DDC.Core.Brine.Lite.Name
import DDC.Core.Brine.Lite.Convert
