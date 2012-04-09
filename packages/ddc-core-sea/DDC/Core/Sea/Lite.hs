
-- | The Lite fragment of Disciple Core.
--
--   This does not support user-defined data types, 
--   but has Units, Ints, Pairs and Lists baked in. 
module DDC.Core.Sea.Lite
        ( -- * Language profile
          profile

          -- * Names of variables and constructors.
        , Name          (..)
        , DataTyCon     (..)
        , PrimDaCon     (..)
        , readName)
where
import DDC.Core.Sea.Lite.Profile
import DDC.Core.Sea.Lite.Name
