
module DDC.Build.Stage.Source.Tetra
        ( ConfigLoadSourceTetra (..)
        , sourceLoad
        , sourceParse
        , sourceDesugar
        , sourceLower)
where
import DDC.Data.Pretty
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import qualified Data.Text                              as Text

import qualified DDC.Data.SourcePos                     as SP
import qualified DDC.Control.Parser                     as Parser

import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Interface.Store              as B
import qualified DDC.Build.Language.Tetra               as BE
import qualified DDC.Build.Stage.Core                   as BC
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
import qualified DDC.Core.Transform.Resolve             as CResolve
import qualified DDC.Core.Transform.SpreadX             as CSpread
import qualified DDC.Core.Transform.Namify              as CNamify


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
        , configSinkResolve      :: B.Sink      -- ^ Sink for core code after resolving.
        , configSinkPreCheck     :: B.Sink      -- ^ Sink for core code before checking.
        , configSinkCheckerTrace :: B.Sink      -- ^ Sink for checker trace.
        , configSinkChecked      :: B.Sink      -- ^ Sink for checked core code.
        , configSinkNamified     :: B.Sink      -- ^ Sink for namified code.
        , configSinkElaborated   :: B.Sink      -- ^ Sink for elaborated core code.
        }


-- | Load source tetra text, desugar and type check it to produce
--   core tetra.
sourceLoad
        :: String                       -- ^ Name of source file.
        -> Int                          -- ^ Line number in source file.
        -> String                       -- ^ Text of source file.
        -> B.Store                      -- ^ Interface store.
        -> ConfigLoadSourceTetra        -- ^ Sinker config.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC Parser.SourcePos CE.Name) CE.Name)

sourceLoad srcName srcLine str store config
 = do
        -- Parse text to source.
        mm_source
         <- sourceParse
                srcName srcLine str
                (configSinkTokens  config)


        -- Desugar source.
        mm_desugared
         <- sourceDesugar
                (configSinkFresh   config)
                (configSinkDefix   config)
                (configSinkExpand  config)
                (configSinkGuards  config)
                (configSinkMatches config)
                (configSinkPrep    config)
                mm_source


        -- Lower source to core.
        mm_core
         <- sourceLower
                store
                (configSinkCore     config)
                (configSinkResolve  config)
                (configSinkPreCheck config)
                mm_desugared


        -- Check core.
        let fragment
                = flip C.mapProfileOfFragment BE.fragment
                $ C.mapFeaturesOfProfile
                $ ( C.setFeature C.ImplicitRun True
                  . C.setFeature C.ImplicitBox True)

        -- Introduce names for anonymous binders.
        --   The resolve transform needs to be able to refer to implicitly
        --   bound parameters by their names, even if there were no names
        --   for them in the source program.
        let profile = C.fragmentProfile  fragment
        let kenv    = C.profilePrimKinds profile
        let tenv    = C.profilePrimTypes profile
        let mm_namified
             = evalState (CNamify.namify (CNamify.makeNamifier (CE.freshT "t$S") kenv)
                                         (CNamify.makeNamifier (CE.freshX "x$S") tenv)
                                        mm_core)
                        (100 :: Int)


        liftIO $ B.pipeSink (renderIndent $ ppr mm_namified)
                            (configSinkNamified config)

        mm_checked
         <- BC.coreCheck
                "SourceLoadText"
                fragment
                (C.Synth [])
                (configSinkCheckerTrace config)
                (configSinkChecked      config)
                mm_namified


        -- Resolve elaborations in module.
        mm_elaborated
         <- do  ntsTop  <- liftIO $ B.importValuesOfStore store
                res     <- liftIO $ CResolve.resolveModule
                                (C.fragmentProfile BE.fragment)
                                ntsTop mm_checked
                case res of
                 Left err  -> throwE [B.ErrorLint "SourceLoadText" "CoreElaborate" err]
                 Right mm' -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_elaborated)
                            (configSinkElaborated config)

        return $ mm_elaborated


---------------------------------------------------------------------------------------------------
-- | Parse a text file into source tetra code.
sourceParse
        :: String               -- ^ Name of source file.
        -> Int                  -- ^ Line number in source file.
        -> String               -- ^ Text of source file.
        -> B.Sink               -- ^ Sink for tokens.
        -> ExceptT [B.Error] IO (S.Module S.Source)

sourceParse
        srcName srcLine str
        sinkTokens
 = do
        -- Lex the input text into source tokens.
        let tokens  = SLexer.lexModuleString srcName srcLine str

        -- Dump tokens to file.
        liftIO $ B.pipeSink
                        (unlines $ map (show . SP.valueOfLocated) $ tokens)
                        sinkTokens

        -- Parse the tokens into a Source Tetra module.
        case Parser.runTokenParser C.describeToken srcName
                        (SParser.pModule) tokens of
         Left err -> throwE [B.ErrorLoad err]
         Right mm -> return mm


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
        -> B.Sink               -- ^ Sink after resolving.
        -> B.Sink               -- ^ Sink after spreading.
        -> S.Module S.Source
        -> ExceptT [B.Error] IO (C.Module SP.SourcePos CE.Name)

sourceLower store sinkCore sinkResolve sinkSpread mm
 = do
        -- Lower source tetra to core tetra.
        let sp          = SP.SourcePos "<top level>" 1 1
        mm_core         <- case SConvert.coreOfSourceModule sp mm of
                            Left err    -> throwE [B.ErrorLoad [err]]
                            Right mm'   -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_core) sinkCore

        -- Resolve which module imported names are from,
        -- and attach arity information to the import statements.
        --
        -- We're currently ignoring the resolver errors as they don't come
        -- with source locations. The type checker will give its own Undefined
        -- variable errors, but not multiple import errors.
        --
        (mm_resolve, _errs_resolve)
         <- liftIO $ BResolve.resolveNamesInModule
                CE.primKindEnv CE.primTypeEnv
                store mm_core

        liftIO $ B.pipeSink (renderIndent $ ppr mm_resolve) sinkResolve

        -- Spread types of data constructors into uses.
        let mm_spread   = CSpread.spreadX CE.primKindEnv CE.primTypeEnv mm_resolve
        liftIO $ B.pipeSink (renderIndent $ ppr mm_spread)  sinkSpread

        return mm_spread


