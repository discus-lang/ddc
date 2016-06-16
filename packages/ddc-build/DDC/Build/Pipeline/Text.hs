{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( PipeText (..)
        , pipeText)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Build.Interface.Store                (Store)
import DDC.Base.Pretty

import qualified DDC.Build.Transform.Resolve       as B

import qualified DDC.Source.Tetra.Convert          as SE
import qualified DDC.Source.Tetra.Transform.Defix  as SE
import qualified DDC.Source.Tetra.Transform.Expand as SE
import qualified DDC.Source.Tetra.Pretty           ()
import qualified DDC.Source.Tetra.Parser           as SE
import qualified DDC.Source.Tetra.Lexer            as SE

import qualified DDC.Build.Language.Tetra          as CE
import qualified DDC.Core.Tetra                    as CE
import qualified DDC.Core.Tetra.Env                as CE

import qualified DDC.Core.Fragment                 as C
import qualified DDC.Core.Transform.SpreadX        as C
import qualified DDC.Core.Check                    as C
import qualified DDC.Core.Load                     as C
import qualified DDC.Core.Lexer                    as C
import qualified DDC.Base.Parser                   as BP
import qualified DDC.Data.SourcePos                as SP
import qualified DDC.Data.Token                    as Token
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
        :: !Sink        -- Sink for source tokens.
        -> !Sink        -- Sink for desugared source code.
        -> !Sink        -- Sink for core tetra code after conversoin.
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
                sinkTokens
                sinkDesugared sinkCore sinkPreCheck sinkCheckerTrace 
                store pipes
         -> {-# SCC "PipeTextLoadSourceTetra" #-}
            let 
                goParse
                 = do   -- Lex the input text into source tokens.
                        let tokens  = SE.lexModuleString srcName srcLine str

                        -- Dump tokens to file.
                        pipeSink (unlines $ map (show . Token.tokenTok) $ tokens) 
                                sinkTokens

                        -- Parse the tokens into a Source Tetra module.
                        case BP.runTokenParser C.describeTok srcName
                                (SE.pModule) tokens of
                         Left err -> return [ErrorLoad err]
                         Right mm -> goDesugar mm

                goDesugar mm
                 =      -- Resolve fixity of infix operators.
                   case SE.defix SE.defaultFixTable mm of
                        Left err  -> return [ErrorLoad err]
                        Right mm' -> goToCore mm'

                goToCore mm
                 = do   let sp            = SP.SourcePos "<top level>" 1 1

                        -- Expand missing quantifiers in signatures.
                        let mm_expand = SE.expandModule sp mm

                        -- Dump desguared source code.
                        pipeSink (renderIndent $ ppr mm_expand) sinkDesugared

                        -- Convert Source Tetra to Core Tetra.
                        -- This source position is used to annotate the 
                        -- let-expression that holds all the top-level bindings.
                        case SE.coreOfSourceModule sp mm_expand of
                         Left err
                          -> return [ErrorLoad err]

                         Right mm_core
                          -> do -- Dump Core Tetra.
                                pipeSink (renderIndent $ ppr mm_core) sinkCore

                                -- Discover which module imported names are from, and
                                -- attach the meta-data which will be needed by follow-on
                                -- compilation, such as the arity of each super.
                                result <- B.resolveNamesInModule 
                                                CE.primKindEnv CE.primTypeEnv
                                                store mm_core

                                case result of 
                                 Left err          -> return [ErrorLoad err]
                                 Right mm_resolved -> goSpread mm_resolved

                goSpread mm
                 = do
                        -- Spread types of data constructors into uses.
                        let mm_spread   = C.spreadX CE.primKindEnv CE.primTypeEnv mm

                        -- Dump loaded code before type checking.
                        pipeSink (renderIndent $ ppr mm_spread) sinkPreCheck

                        -- Type check the code, synthesising missing type annotations.
                        --  Insert casts to implicitly run suspended bindings along the way.
                        let fragment_implicit
                                = flip C.mapProfileOfFragment CE.fragment
                                $ C.mapFeaturesOfProfile 
                                $ ( C.setFeature C.ImplicitRun True
                                  . C.setFeature C.ImplicitBox True)

                        pipeCore mm_spread
                          $ PipeCoreCheck fragment_implicit C.Synth sinkCheckerTrace pipes

            in goParse

 where  sinkCheckTrace mct sink
         = case mct of
                Nothing                 -> return []
                Just (C.CheckTrace doc) -> pipeSink (renderIndent doc) sink


