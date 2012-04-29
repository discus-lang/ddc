
-- | The Lite fragment of Disciple Core.
--
--   This does not support user-defined data types, 
--   but has Units, Ints, Pairs and Lists baked in. 
module DDC.Core.Brine.Lite
        ( -- * Language profile
          profile
        , lexModuleString
        , lexExpString

          -- * Conversion to the Disciple Core Brine.
        , toBrine

          -- * Names of variables and constructors.
        , Name          (..)
        , DataTyCon     (..)
        , PrimDaCon     (..)
        , readName)
where
import DDC.Core.Brine.Lite.Profile
import DDC.Core.Brine.Lite.Name
import DDC.Core.Brine.Lite.Convert
