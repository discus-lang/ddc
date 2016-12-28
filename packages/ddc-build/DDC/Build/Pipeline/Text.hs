{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( PipeText (..)
        , pipeText)
where
import DDC.Build.Stage.Source.Tetra
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Build.Interface.Store                        (Store)
import DDC.Data.Pretty
import Control.Monad.Trans.Except

import qualified DDC.Source.Tetra.Pretty                ()

import qualified DDC.Core.Tetra                         as CE
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Load                          as C
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

  PipeTextLoadSourceTetra
        :: !Sink        -- Sink for source tokens.
        -> !Sink        -- Sink for parsed source code.
        -> !Sink        -- Sink for freshened code.
        -> !Sink        -- Sink for defixed source code.
        -> !Sink        -- Sink for expanded source code.
        -> !Sink        -- Sink for guard desugared source code.
        -> !Sink        -- Sink for match desugared source code.
        -> !Sink        -- Sink for prepped source code.
        -> !Sink        -- Sink for core tetra code after conversion.
        -> !Sink        -- Sink for core tetra code before type checking.
        -> !Sink        -- Sink for type checker trace.
        -> !Store       -- Interface store.
        -> ![PipeCore (C.AnTEC BP.SourcePos CE.Name) CE.Name]
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

-------------------------------------------------------------------------------
pipeText !srcName !srcLine !str 
         !(PipeTextLoadCore !fragment !mode !sink !pipes)
 = {-# SCC "PipeTextLoadCore" #-}
   do   let   toks    = fragmentLexModule fragment srcName srcLine str 

--        putStrLn $ unlines $ map (show . SP.valueOfLocated) toks

        case C.loadModuleFromTokens fragment srcName mode toks of
          (Left err, mct) 
           -> do sinkCheckTrace mct sink
                 return [ErrorLoad err]

          (Right mm, mct) 
           -> do sinkCheckTrace mct sink
                 pipeCores mm pipes

 where  sinkCheckTrace mct sink'
         = case mct of
                Nothing                 -> return []
                Just (C.CheckTrace doc) -> pipeSink (renderIndent doc) sink'

pipeText !srcName !srcLine !str 
         (PipeTextLoadSourceTetra 
                sinkTokens sinkParsed sinkFresh
                sinkDefix  sinkExpand sinkGuards sinkMatches sinkPrep
                sinkCore        
                sinkPreCheck sinkCheckerTrace 
                store pipes)
 = do   
        result  <- runExceptT 
                $  sourceLoad srcName srcLine str store 
                $  ConfigLoadSourceTetra
                        { configSinkTokens              = sinkTokens
                        , configSinkParsed              = sinkParsed
                        , configSinkFresh               = sinkFresh
                        , configSinkDefix               = sinkDefix
                        , configSinkExpand              = sinkExpand
                        , configSinkGuards              = sinkGuards
                        , configSinkMatches             = sinkMatches
                        , configSinkPrep                = sinkPrep
                        , configSinkCore                = sinkCore
                        , configSinkPreCheck            = sinkPreCheck
                        , configSinkCheckerTrace        = sinkCheckerTrace 
                        , configSinkChecked             = SinkDiscard 
                        , configSinkElaborated          = SinkDiscard }

        case result of
         Left errs      -> return errs
         Right mm       -> fmap concat $ mapM (pipeCore mm) pipes


