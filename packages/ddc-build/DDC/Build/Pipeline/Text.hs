{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( InterfaceAA
        , PipeText (..)
        , pipeText)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Build.Interface.Base
import DDC.Base.Pretty
import Data.Maybe

import qualified DDC.Source.Tetra.ToCore           as SE
import qualified DDC.Source.Tetra.Transform.Defix  as SE
import qualified DDC.Source.Tetra.Transform.Expand as SE
import qualified DDC.Source.Tetra.Parser           as SE
import qualified DDC.Source.Tetra.Lexer            as SE
import qualified DDC.Source.Tetra.Env              as SE

import qualified DDC.Build.Language.Tetra          as CE
import qualified DDC.Core.Tetra                    as CE
import qualified DDC.Core.Tetra.Env                as CE

import qualified DDC.Core.Parser                   as C
import qualified DDC.Core.Transform.Resolve        as C
import qualified DDC.Core.Transform.SpreadX        as C
import qualified DDC.Core.Check                    as C
import qualified DDC.Core.Load                     as C
import qualified DDC.Core.Lexer                    as C
import qualified DDC.Base.Parser                   as BP
import qualified DDC.Data.SourcePos                as SP

import qualified Data.Map                          as Map
import Control.DeepSeq

type InterfaceAA        
        = Interface (C.AnTEC BP.SourcePos CE.Name) ()

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
        :: !Sink                -- Sink for source tokens.
        -> !Sink                -- Sink for core code before final type checking.
        -> !Sink                -- Sink for type checker trace.
        -> ![InterfaceAA]       -- Interfaces for modules upon which this one depends.
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
                        return [ErrorLoad err]

                 (Right mm, mct) 
                  -> do sinkCheckTrace mct sink
                        pipeCores mm pipes

        PipeTextLoadSourceTetra 
                sinkTokens sinkPreCheck sinkCheckerTrace 
                interfaces
                pipes
         -> {-# SCC "PipeTextLoadSourceTetra" #-}
            let goParse
                 = do   -- Lex the input text into source tokens.
                        let tokens  = SE.lexModuleString srcName srcLine str

                        pipeSink (unlines $ map show $ tokens) sinkTokens

                        -- Parse the source tokens.
                        let context = C.Context
                                { C.contextTrackedEffects         = True
                                , C.contextTrackedClosures        = True
                                , C.contextFunctionalEffects      = False
                                , C.contextFunctionalClosures     = False }

                        case BP.runTokenParser C.describeTok srcName
                                (SE.pModule context) tokens of
                         Left err -> return [ErrorLoad err]
                         Right mm -> goDesugar mm

                goDesugar mm
                 = case SE.defix SE.defaultFixTable mm of
                        Left err  -> return [ErrorLoad err]
                        Right mm' -> goToCore mm'

                goToCore mm
                 = do   -- Expand missing quantifiers in signatures.
                        let mm_expand = SE.expand SE.configDefault 
                                            SE.primKindEnv SE.primTypeEnv mm

                        -- Convert Source Tetra to Core Tetra.
                        -- This source position is used to annotate the let expression
                        -- that holds all the top-level bindings.
                        let sp        = SP.SourcePos "<top level>" 1 1
                        let mm_core   = SE.toCoreModule sp mm_expand

                        -- Resolve references to imported types and bindings.
                        let deps      = Map.fromList
                                      $ catMaybes
                                      $ [ case interfaceTetraModule i of
                                                Nothing -> Nothing
                                                Just tm -> Just (interfaceModuleName i, tm)
                                        | i <- interfaces ]
                        let mm_resolve 
                                = C.resolveNamesInModule 
                                        CE.primKindEnv CE.primTypeEnv
                                        deps mm_core

                        -- Spread types of data constructors into uses.
                        let mm_spread  
                                = C.spreadX
                                        CE.primKindEnv CE.primTypeEnv
                                        mm_resolve

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





