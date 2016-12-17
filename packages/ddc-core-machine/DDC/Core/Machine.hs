
-- | Disciple Core Flow is a Domain Specific Language (DSL) for writing first
--   order data flow programs.
--   
module DDC.Core.Machine
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , KiConMachine  (..)
        , TyConMachine  (..)
        , OpMachine     (..)

          -- * Name Parsing
        , readName

          -- * Program Lexing
        , lexModuleString
        , lexExpString

          -- * Processes
        , Process       (..)
        , Network       (..)
        , slurpNetworks
        )

where
import DDC.Core.Machine.Prim
import DDC.Core.Machine.Profile
import DDC.Core.Machine.Process
