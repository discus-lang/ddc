{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Text
        ( PipeText (..)
        , pipeText)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Core
import DDC.Build.Language
import DDC.Build.Interface.Store                        (Store)
import DDC.Base.Pretty

import qualified DDC.Build.Transform.Resolve            as B

import qualified DDC.Source.Tetra.Convert               as SConvert
import qualified DDC.Source.Tetra.Transform.Freshen     as SFreshen
import qualified DDC.Source.Tetra.Transform.Defix       as SDefix
import qualified DDC.Source.Tetra.Transform.Expand      as SExpand
import qualified DDC.Source.Tetra.Transform.Guards      as SGuards
import qualified DDC.Source.Tetra.Transform.Matches     as SMatches
import qualified DDC.Source.Tetra.Transform.Prep        as SPrep
import qualified DDC.Source.Tetra.Parser                as SParser
import qualified DDC.Source.Tetra.Lexer                 as SLexer
import qualified DDC.Source.Tetra.Pretty                ()

import qualified DDC.Build.Language.Tetra               as CE
import qualified DDC.Core.Tetra                         as CE
import qualified DDC.Core.Tetra.Env                     as CE

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Transform.SpreadX             as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Load                          as C
import qualified DDC.Core.Lexer                         as C
import qualified DDC.Base.Parser                        as BP
import qualified DDC.Data.SourcePos                     as SP

import qualified Data.Text                              as Text
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
pipeText !_srcName !_srcLine !str 
         !(PipeTextOutput !sink)
 = {-# SCC "PipeTextOutput" #-}
   pipeSink str sink


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


-------------------------------------------------------------------------------
pipeText !srcName !srcLine !str
         (PipeTextLoadSourceTetra 
                sinkTokens sinkParsed sinkFresh
                sinkDefix  sinkExpand sinkGuards sinkMatches sinkPrep
                sinkCore        
                sinkPreCheck sinkCheckerTrace 
                store pipes)
 = goParse
 where 
        goParse
         = do   -- Lex the input text into source tokens.
                let tokens  = SLexer.lexModuleString srcName srcLine str

                -- Dump tokens to file.
                pipeSink (unlines $ map (show . SP.valueOfLocated) $ tokens) 
                        sinkTokens

                -- Parse the tokens into a Source Tetra module.
                case BP.runTokenParser C.describeToken srcName
                        (SParser.pModule) tokens of
                 Left err -> return [ErrorLoad err]
                 Right mm 
                  -> do pipeSink (renderIndent $ ppr mm) sinkParsed
                        goDesugar mm

        goDesugar mm
         = do   -- Freshen shadowed names and eliminate anonymous binders.
                let mm_fresh    = SFreshen.evalState (Text.pack "f")
                                $ SFreshen.freshenModule mm
                pipeSink (renderIndent $ ppr mm_fresh) sinkFresh

              -- Resolve fixity of infix operators.
                case SDefix.defix SDefix.defaultFixTable mm_fresh of
                 Left err  -> return [ErrorLoad err]
                 Right mm' -> goToCore mm'

        goToCore mm_defixed
         = do   -- Dump defixed source code.
                pipeSink (renderIndent $ ppr mm_defixed) sinkDefix

                -- Expand missing quantifiers in signatures.
                let sp          = SP.SourcePos "<top level>" 1 1
                let mm_expand   = SExpand.expandModule sp mm_defixed
                pipeSink (renderIndent $ ppr mm_expand)  sinkExpand

                -- Desugar guards and patterns to match expressions.
                let mm_guards   = SGuards.evalState   (Text.pack "g")
                                $ SGuards.desugarModule mm_expand
                pipeSink (renderIndent $ ppr mm_guards) sinkGuards

                -- Desugar match expressions to case expressions.
                let mm_match    = SMatches.evalState  (Text.pack "m")
                                $ SMatches.desugarModule mm_guards
                pipeSink (renderIndent $ ppr mm_match)  sinkMatches

                -- Prepare for conversion to core.
                let mm_prep     = SPrep.evalState     (Text.pack "p")
                                $ SPrep.desugarModule mm_match
                pipeSink (renderIndent $ ppr mm_prep)   sinkPrep

                -- Convert Source Tetra to Core Tetra.
                -- This source position is used to annotate the 
                -- let-expression that holds all the top-level bindings.
                case SConvert.coreOfSourceModule sp mm_prep of
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

