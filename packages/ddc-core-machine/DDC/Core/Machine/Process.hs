
module DDC.Core.Machine.Process
        ( Label         (..)
        , Channel       (..)
        , ChannelType   (..)
        , BlockNext     (..)
        , Block         (..)
        , Process       (..)
        , Network       (..)
        , SlurpError    (..)
        , slurpNetworks
        , fuseNetwork
        )
where
import DDC.Core.Machine.Process.Base
import DDC.Core.Machine.Process.Slurp
import DDC.Core.Machine.Process.Fuse


