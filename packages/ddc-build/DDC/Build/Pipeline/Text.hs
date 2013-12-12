{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( PipeText (..)
        , pipeText)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Base.Pretty
import qualified DDC.Base.Parser        as BP
import qualified DDC.Core.Check         as C
import qualified DDC.Core.Load          as CL
import Control.DeepSeq


-- | Process program text.
data PipeText n (err :: * -> *) where
  PipeTextOutput 
        :: !Sink
        -> PipeText n err

  PipeTextLoadCore 
        :: (Ord n, Show n, Pretty n)
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

pipeText !srcName !srcLine !str !pp
 = case pp of
        PipeTextOutput !sink
         -> {-# SCC "PipeTextOutput" #-}
            pipeSink str sink

        PipeTextLoadCore !frag !mode !sink !pipes
         -> {-# SCC "PipeTextLoadCore" #-}
            let toks    = fragmentLexModule frag srcName srcLine str in
            let profile = fragmentProfile frag
            in case CL.loadModuleFromTokens profile srcName mode toks of
                 (Left err, mct) 
                  -> do sinkCheckTrace mct sink
                        return $ [ErrorLoad err]

                 (Right mm, mct) 
                  -> do sinkCheckTrace mct sink
                        pipeCores mm pipes


 where  sinkCheckTrace mct sink
         = case mct of
                Nothing                  -> return []
                Just (CL.CheckTrace doc) -> pipeSink (renderIndent doc) sink

