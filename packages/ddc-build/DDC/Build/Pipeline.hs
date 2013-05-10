{-# LANGUAGE GADTs #-}
-- | A pipeline is an abstraction of a single compiler pass.
--
--  NOTE: The Haddock documentation on pipeline constructors is missing
--        because Haddock does not support commenting GADTs.
--        See the source code for documentation.
--
module DDC.Build.Pipeline
        ( -- * Errors
          Error(..)

          -- * Source code
        , PipeText        (..)
        , pipeText

          -- * Generic Core modules
        , PipeCore        (..)
        , pipeCore

          -- * Core Lite modules
        , PipeLite        (..)
        , pipeLite

          -- * Core Salt modules
        , PipeSalt        (..)
        , pipeSalt

          -- * LLVM modules
        , PipeLlvm        (..)
        , pipeLlvm

          -- * Emitting output
        , Sink                  (..)
        , pipeSink)
where
import DDC.Build.Pipeline.Text
import DDC.Build.Pipeline.Core
import DDC.Build.Pipeline.Salt
import DDC.Build.Pipeline.Llvm
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Error

