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

import qualified DDC.Source.Tetra.ToCore        as SE
import qualified DDC.Source.Tetra.Desugar.Defix as SE
import qualified DDC.Source.Tetra.Infer.Expand  as SE
import qualified DDC.Source.Tetra.Parser        as SE
import qualified DDC.Source.Tetra.Lexer         as SE
import qualified DDC.Source.Tetra.Env           as SE

import qualified DDC.Build.Language.Tetra       as CE
import qualified DDC.Core.Tetra                 as CE
import qualified DDC.Core.Tetra.Env             as CE

import qualified DDC.Core.Parser.Context        as C
import qualified DDC.Core.Transform.SpreadX     as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Load                  as C
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
import qualified DDC.Data.SourcePos             as SP
import Control.DeepSeq


-- | Process program text.
data PipeText n (err :: * -> *) where
  PipeTextOutput 
        :: !Sink
        -> PipeText n err

  PipeTextLoadCore 
        :: (Ord n, Show n, Pretty n, Pretty (err (C.AnTEC SP.SourcePos n)))
        => !(Fragment n err)
        -> !(C.Mode n)
        -> !Sink
        -> ![PipeCore (C.AnTEC BP.SourcePos n) n]
        -> PipeText n err

  PipeTextLoadSourceTetra
        :: !Sink                        -- Sink for core code before final type checking.
        -> !Sink                        -- Sink for type checker trace.
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

pipeText !srcName !srcLine !str !pp
 = case pp of
        PipeTextOutput !sink
         -> {-# SCC "PipeTextOutput" #-}
            pipeSink str sink

        PipeTextLoadCore !fragment !mode !sink !pipes
         -> {-# SCC "PipeTextLoadCore" #-}
            let toks    = fragmentLexModule fragment srcName srcLine str 
            in case C.loadModuleFromTokens fragment srcName mode toks of
                 (Left err, mct) 
                  -> do sinkCheckTrace mct sink
                        return $ [ErrorLoad err]

                 (Right mm, mct) 
                  -> do sinkCheckTrace mct sink
                        pipeCores mm pipes

        PipeTextLoadSourceTetra sinkPreCheck sinkCheckerTrace pipes
         -> {-# SCC "PipeTextLoadSourceTetra" #-}
            let goParse
                 = let  -- Lex the input text into source tokens.
                        tokens  = SE.lexModuleString srcName srcLine str

                        -- Parse the source tokens.
                        context = C.Context
                                { C.contextTrackedEffects         = True
                                , C.contextTrackedClosures        = True
                                , C.contextFunctionalEffects      = False
                                , C.contextFunctionalClosures     = False }

                    in  case BP.runTokenParser C.describeTok srcName
                                (SE.pModule context) tokens of
                         Left err -> error $ show err    -- TODO: throw errorLoad instead.
                         Right mm -> goDesugar mm

                goDesugar mm
                 = case SE.defix SE.defaultFixTable mm of
                        Left err  -> error $ show err    -- TODO: return errorLoad instead.
                        Right mm' -> goToCore mm'

                goToCore mm
                 = do   -- Expand missing quantifiers in signatures.
                        let mm_expand = SE.expand SE.configDefault 
                                            SE.primKindEnv SE.primTypeEnv mm

                        -- Convert Source Tetra to Core Tetra.
                        -- TODO get proper source location.
                        let sp        = SP.SourcePos "<top level>" 1 1
                        let mm_core   = SE.toCoreModule sp mm_expand

                        -- Spread types of data constructors into uses.
                        let mm_spread = C.spreadX 
                                          CE.primKindEnv CE.primTypeEnv mm_core

                        -- Dump code before checking for debugging purposes.
                        pipeSink (renderIndent $ ppr mm_spread) sinkPreCheck

                        -- Use the existing checker pipeline to Synthesise
                        -- missing type annotations.
                        pipeCore mm_spread
                           $ PipeCoreCheck CE.fragment C.Synth sinkCheckerTrace pipes

            in goParse

 where  sinkCheckTrace mct sink
         = case mct of
                Nothing                 -> return []
                Just (C.CheckTrace doc) -> pipeSink (renderIndent doc) sink





