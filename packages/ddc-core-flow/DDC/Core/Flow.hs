
-- | Disciple Core Flow is a Domain Specific Language (DSL) for writing first
--   order data flow programs.
--   
--   This package provides the language definition as a fragment of Disciple
--   Core. It also provides an implementation of the lowering transform which
--   converts data flow programs into imperative nested loop code.
--
--   The @repa-plugin@ package provides a GHC plugin that transforms GHC core
--   programs gained from vanilla Haskell sources. Use this package if you
--   just want to write and run real programs.
--
--   Alternatively, Disciple Core Flow programs can be transformed directly
--   via the @ddc@ or @ddci-core@ command line interfaces, but DDC itself
--   doesn't provide full compilation to machine code. Use GHC and the 
--   @repa-plugin@ for that.
-- 
module DDC.Core.Flow
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConFlow     (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..)

          -- * Name Parsing
        , readName

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Profile
