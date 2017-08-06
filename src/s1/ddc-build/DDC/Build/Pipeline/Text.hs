{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( PipeText (..)
        , pipeText)
where
import qualified DDC.Build.Stage.Core                   as BC

import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Data.Pretty
import Control.Monad.Trans.Except

import qualified DDC.Source.Tetra.Pretty                ()

import qualified DDC.Core.Check                         as C
import qualified DDC.Control.Parser                     as BP
import qualified DDC.Data.SourcePos                     as SP

import Control.DeepSeq


-- | Process program text.
data PipeText n (err :: * -> *) where
  PipeTextLoadCore
        :: (Ord n, Show n, Pretty n, Pretty (err (C.AnTEC SP.SourcePos n)))
        => !(Fragment n err)
        -> !(C.Mode n)
        -> !Sink
        -> ![PipeCore (C.AnTEC BP.SourcePos n) n]
        -> PipeText n err


-- | Process a text module.
--
--   Returns empty list on success.
pipeText
        :: NFData n
        => String
        -> Int
        -> String
        -> PipeText n err
        -> IO [Error]

pipeText !srcName !srcLine !str
         !(PipeTextLoadCore !fragment !mode !sink !pipes)
 = do
        result  <- runExceptT
                $  BC.coreLoad "TextLoadCore" fragment mode srcName srcLine str
                $  BC.ConfigCoreLoad
                        { BC.configSinkTokens           = SinkDiscard
                        , BC.configSinkParsed           = sink
                        , BC.configSinkChecked          = SinkDiscard
                        , BC.configSinkTrace            = SinkDiscard }

        case result of
         Left errs      -> return errs
         Right mm       -> fmap concat $ mapM (pipeCore mm) pipes


