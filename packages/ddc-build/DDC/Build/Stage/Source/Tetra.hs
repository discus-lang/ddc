
module DDC.Build.Stage.Source.Tetra
        ( ConfigLoadSourceTetra (..)
        , sourceLoadText

        , sourceParseText
        , sourceDesugar
        , sourceLower)
where
import DDC.Data.Pretty
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import qualified Data.Text                              as Text

import qualified DDC.Data.SourcePos                     as SP
import qualified DDC.Control.Parser                     as Parser

import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Interface.Store              as B
import qualified DDC.Build.Language.Tetra               as BE
import qualified DDC.Build.Stage.Core                   as BS

import qualified DDC.Build.Transform.Resolve            as BResolve

import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Module                as S

import qualified DDC.Source.Tetra.Transform.Freshen     as SFreshen
import qualified DDC.Source.Tetra.Transform.Defix       as SDefix
import qualified DDC.Source.Tetra.Transform.Expand      as SExpand
import qualified DDC.Source.Tetra.Transform.Guards      as SGuards
import qualified DDC.Source.Tetra.Transform.Matches     as SMatches
import qualified DDC.Source.Tetra.Transform.Prep        as SPrep
import qualified DDC.Source.Tetra.Convert               as SConvert
import qualified DDC.Source.Tetra.Parser                as SParser
import qualified DDC.Source.Tetra.Lexer                 as SLexer

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Lexer                         as C

import qualified DDC.Core.Tetra                         as CE
import qualified DDC.Core.Tetra.Env                     as CE

import qualified DDC.Core.Transform.SpreadX             as CSpread


---------------------------------------------------------------------------------------------------
data ConfigLoadSourceTetra
        = ConfigLoadSourceTetra
        { configSinkTokens       :: B.Sink      -- ^ Sink for source tokens.
        , configSinkParsed       :: B.Sink      -- ^ Sink for parsed source code.
        , configSinkFresh        :: B.Sink      -- ^ Sink for freshened code.
        , configSinkDefix        :: B.Sink      -- ^ Sink for defixed source code.
        , configSinkExpand       :: B.Sink      -- ^ Sink for expanded source code.
        , configSinkGuards       :: B.Sink      -- ^ Sink for guard desugared code.
        , configSinkMatches      :: B.Sink      -- ^ Sink for match desugared code.
        , configSinkPrep         :: B.Sink      -- ^ Sink for code before conversion.
        , configSinkCore         :: B.Sink      -- ^ Sink for core code.
        , configSinkPreCheck     :: B.Sink      -- ^ Sink for core code before checking.
        , configSinkCheckerTrace :: B.Sink      -- ^ Sink for checker trace.
        , configSinkChecked      :: B.Sink      -- ^ Sink for checked core code.
        }


-- | Load source tetra text, desugar and type check it to produce
--   core tetra.
sourceLoadText
        :: String                       -- ^ Name of source file.
        -> Int                          -- ^ Line number in source file.
        -> String                       -- ^ Text of source file.
        -> B.Store                      -- ^ Interface store.
        -> ConfigLoadSourceTetra        -- ^ Sinker config.
        -> IO (Either [B.Error]
                      (C.Module (C.AnTEC SP.SourcePos CE.Name) CE.Name))
        
sourceLoadText srcName srcLine str store config
 = runExceptT goParse
 where 
        goParse 
         =   sourceParseText 
                srcName srcLine str
                (configSinkTokens config)
                (configSinkParsed config)

         >>= sourceDesugar 
                (configSinkFresh   config)
                (configSinkDefix   config)
                (configSinkExpand  config)
                (configSinkGuards  config)
                (configSinkMatches config)
                (configSinkPrep    config)

         >>= sourceLower 
                store
                (configSinkCore     config)
                (configSinkPreCheck config)

         >>= BS.coreCheck 
                "sourceLoadText"
                fragment_implicit
                (C.Synth [])
                (configSinkCheckerTrace config)
                (configSinkChecked      config)

        -- Type check the code, synthesising missing type annotation    s.
        --  Insert casts to implicitly run suspended bindings along the way.
        fragment_implicit
                = flip C.mapProfileOfFragment BE.fragment
                $ C.mapFeaturesOfProfile 
                $ ( C.setFeature C.ImplicitRun True
                  . C.setFeature C.ImplicitBox True)


---------------------------------------------------------------------------------------------------
-- | Parse a text file into source tetra code.
sourceParseText
        :: String               -- ^ Name of source file.
        -> Int                  -- ^ Line number in source file.
        -> String               -- ^ Text of source file.
        -> B.Sink               -- ^ Sink for tokens.
        -> B.Sink               -- ^ Sink for parsed source.
        -> ExceptT [B.Error] IO (S.Module S.Source)

sourceParseText
        srcName srcLine str
        sinkTokens sinkSource
 = do   
        -- Lex the input text into source tokens.
        let tokens  = SLexer.lexModuleString srcName srcLine str

        -- Dump tokens to file.
        liftIO $ B.pipeSink (unlines $ map (show . SP.valueOfLocated) $ tokens) 
                        sinkTokens

        -- Parse the tokens into a Source Tetra module.
        case Parser.runTokenParser C.describeToken srcName 
                        (SParser.pModule) tokens of
         Left err 
          ->    throwE [B.ErrorLoad err]

         Right mm 
          -> do liftIO $ B.pipeSink (renderIndent $ ppr mm) 
                        sinkSource
                return mm


---------------------------------------------------------------------------------------------------
-- | Desugar source tetra code and prepare for conversion to core.
sourceDesugar
        :: B.Sink               -- ^ Sink after name freshening.
        -> B.Sink               -- ^ Sink after desugaring infix expressions.
        -> B.Sink               -- ^ Sink after expanding missing quantifiers.
        -> B.Sink               -- ^ Sink after desugaring guards.
        -> B.Sink               -- ^ Sink after desugaring matches.
        -> B.Sink               -- ^ Sink after prep for conversion to core.
        -> S.Module S.Source
        -> ExceptT [B.Error] IO (S.Module S.Source)

sourceDesugar
        sinkFresh  sinkDefix   sinkExpand
        sinkGuards sinkMatches sinkPrep
        mm
 = do   
        -- Freshen shadowed names and eliminate anonymous binders.
        let mm_fresh    = SFreshen.evalState (Text.pack "f")
                        $ SFreshen.freshenModule mm

        liftIO $ B.pipeSink (renderIndent $ ppr mm_fresh)   sinkFresh


        -- Resolve fixity of infix operators.
        let result_defixed = SDefix.defix SDefix.defaultFixTable mm_fresh 
        mm_defixed      <- case result_defixed of
                            Left err        -> throwE [B.ErrorLoad [err]]
                            Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_defixed) sinkDefix


        -- Expand missing quantifiers in signatures.
        let sp          = SP.SourcePos "<top level>" 1 1
        let mm_expand   = SExpand.expandModule sp mm_defixed
        liftIO $ B.pipeSink (renderIndent $ ppr mm_expand)  sinkExpand


        -- Desugar guards and patterns to match expressions.
        let mm_guards   = SGuards.evalState   (Text.pack "g")
                        $ SGuards.desugarModule mm_expand
        liftIO $ B.pipeSink (renderIndent $ ppr mm_guards)  sinkGuards


        -- Desugar match expressions to case expressions.
        let mm_match    = SMatches.evalState  (Text.pack "m")
                        $ SMatches.desugarModule mm_guards
        liftIO $ B.pipeSink (renderIndent $ ppr mm_match)   sinkMatches

                
        -- Prepare for conversion to core.
        let mm_prep     = SPrep.evalState     (Text.pack "p")
                        $ SPrep.desugarModule mm_match
        liftIO $ B.pipeSink (renderIndent $ ppr mm_prep)    sinkPrep

        return mm_prep


---------------------------------------------------------------------------------------------------
-- | Lower desugared source tetra code to core tetra.
sourceLower 
        :: B.Store              -- ^ Interface store.
        -> B.Sink               -- ^ Sink after conversion to core.
        -> B.Sink               -- ^ Sink after spreading.
        -> S.Module S.Source
        -> ExceptT [B.Error] IO (C.Module SP.SourcePos CE.Name)

sourceLower store sinkCore sinkSpread mm
 = do   
        -- Lower source tetra to core tetra.
        let sp          = SP.SourcePos "<top level>" 1 1
        mm_core         <- case SConvert.coreOfSourceModule sp mm of
                            Left err    -> throwE [B.ErrorLoad [err]]
                            Right mm'   -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_core) sinkCore


        -- Resolve which module imported names are from, 
        -- and attach arity information to the import statements.
        result_resolve  <- liftIO $ BResolve.resolveNamesInModule 
                                        CE.primKindEnv CE.primTypeEnv
                                        store mm_core

        mm_resolve      <- case result_resolve of
                            Left err    -> throwE [B.ErrorLoad [err]]
                            Right mm'   -> return mm'

        -- TODO: dump resolved

        -- Spread types of data constructors into uses.
        let mm_spread   = CSpread.spreadX CE.primKindEnv CE.primTypeEnv mm_resolve
        liftIO $ B.pipeSink (renderIndent $ ppr mm_spread) sinkSpread

        return mm_spread


