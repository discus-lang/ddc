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
import DDC.Data.Pretty

import qualified DDC.Build.Transform.Resolve            as B

import qualified DDC.Source.Tetra.Exp                   as S
import qualified DDC.Source.Tetra.Module                as S
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
import qualified DDC.Core.Module                        as C
import qualified DDC.Control.Parser                     as BP
import qualified DDC.Data.SourcePos                     as SP

import qualified Data.Text                              as Text
import Control.DeepSeq
import Data.IORef
import DDC.Data.Canned
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad


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
        result  <- loadSourceTetraOfText srcName srcLine str store 
                $  ConfigLoadSourceTetra
                        { configSinkTokens             = sinkTokens
                        , configSinkParsed             = sinkParsed
                        , configSinkFresh              = sinkFresh
                        , configSinkDefix              = sinkDefix
                        , configSinkExpand             = sinkExpand
                        , configSinkGuards             = sinkGuards
                        , configSinkMatches            = sinkMatches
                        , configSinkPrep               = sinkPrep
                        , configSinkCore               = sinkCore
                        , configSinkPreCheck           = sinkPreCheck
                        , configSinkCheckerTrace       = sinkCheckerTrace }

        case result of
         Left errs      -> return errs
         Right mm       -> fmap concat $ mapM (pipeCore mm) pipes


-------------------------------------------------------------------------------
data ConfigLoadSourceTetra
        = ConfigLoadSourceTetra
        { configSinkTokens       :: Sink    -- ^ Sink for source tokens.
        , configSinkParsed       :: Sink    -- ^ Sink for parsed source code.
        , configSinkFresh        :: Sink    -- ^ Sink for freshened code.
        , configSinkDefix        :: Sink    -- ^ Sink for defixed source code.
        , configSinkExpand       :: Sink    -- ^ Sink for expanded source code.
        , configSinkGuards       :: Sink    -- ^ Sink for guard desugared code.
        , configSinkMatches      :: Sink    -- ^ Sink for match desugared code.
        , configSinkPrep         :: Sink    -- ^ Sink for code before conversion.
        , configSinkCore         :: Sink    -- ^ Sink for core code.
        , configSinkPreCheck     :: Sink    -- ^ Sink for core code before checking.
        , configSinkCheckerTrace :: Sink    -- ^ Sink for checker trace.
        }


loadSourceTetraOfText
        :: String                       -- ^ Name of source file.
        -> Int                          -- ^ Line number in source file.
        -> String                       -- ^ Text of source file.
        -> Store                        -- ^ Interface store.
        -> ConfigLoadSourceTetra        -- ^ Sinker config.
        -> IO (Either [Error]
                      (C.Module (C.AnTEC BP.SourcePos CE.Name) CE.Name))
        
loadSourceTetraOfText srcName srcLine str store config
 = fmap join $ runExceptT goParse
 where 
        goParse 
         =   parseSourceTetra 
                (configSinkTokens config)
                (configSinkParsed config)
                srcName srcLine str 

         >>= desugarSourceTetra 
                (configSinkFresh   config)
                (configSinkDefix   config)
                (configSinkExpand  config)
                (configSinkGuards  config)
                (configSinkMatches config)
                (configSinkPrep    config)

         >>= goLower 


        goLower mm
         = do   mm'     <- lowerSourceTetra 
                                (configSinkCore     config)
                                (configSinkPreCheck config)
                                store mm

                liftIO $ goCheck mm'

        goCheck mm
                -- Type check the code, synthesising missing type annotations.
                --  Insert casts to implicitly run suspended bindings along the way.
         = do   let fragment_implicit
                        = flip C.mapProfileOfFragment CE.fragment
                        $ C.mapFeaturesOfProfile 
                        $ ( C.setFeature C.ImplicitRun True
                          . C.setFeature C.ImplicitBox True)

                refOut <- newIORef Nothing

                errs    <- pipeCore mm
                        $ PipeCoreCheck "pipeText" fragment_implicit (C.Synth []) 
                              (configSinkCheckerTrace config)
                          [ PipeCoreHacks (Canned $ \mm' -> writeIORef refOut (Just mm') >> return mm')
                            []]

                mout   <- readIORef refOut
                case mout of
                 Nothing -> return $ Left  errs
                 Just out
                  -> case errs of
                        []      -> return $ Right out
                        _       -> return $ Left errs



-------------------------------------------------------------------------------
-- | Parse a text file into source tetra code.
parseSourceTetra
        :: Sink                 -- ^ Sink for tokens.
        -> Sink                 -- ^ Sink for parsed source.
        -> String               -- ^ Name of source file.
        -> Int                  -- ^ Line number in source file.
        -> String               -- ^ Text of source file.
        -> ExceptT [Error] IO (S.Module S.Source)

parseSourceTetra 
        sinkTokens sinkSource
        srcName srcLine str
 = do   
        -- Lex the input text into source tokens.
        let tokens  = SLexer.lexModuleString srcName srcLine str

        -- Dump tokens to file.
        liftIO $ pipeSink (unlines $ map (show . SP.valueOfLocated) $ tokens) 
                        sinkTokens

        -- Parse the tokens into a Source Tetra module.
        case BP.runTokenParser C.describeToken srcName 
                        (SParser.pModule) tokens of
         Left err 
          ->    throwE [ErrorLoad err]

         Right mm 
          -> do liftIO $ pipeSink (renderIndent $ ppr mm) 
                        sinkSource
                return mm


-------------------------------------------------------------------------------
-- | Desugar source tetra code and prepare for conversion to core.
desugarSourceTetra
        :: Sink                 -- ^ Sink after name freshening.
        -> Sink                 -- ^ Sink after desugaring infix expressions.
        -> Sink                 -- ^ Sink after expanding missing quantifiers.
        -> Sink                 -- ^ Sink after desugaring guards.
        -> Sink                 -- ^ Sink after desugaring matches.
        -> Sink                 -- ^ Sink after prep for conversion to core.
        -> S.Module S.Source
        -> ExceptT [Error] IO (S.Module S.Source)

desugarSourceTetra 
        sinkFresh  sinkDefix   sinkExpand
        sinkGuards sinkMatches sinkPrep
        mm
 = do   
        -- Freshen shadowed names and eliminate anonymous binders.
        let mm_fresh    = SFreshen.evalState (Text.pack "f")
                        $ SFreshen.freshenModule mm

        liftIO $ pipeSink (renderIndent $ ppr mm_fresh)   sinkFresh


        -- Resolve fixity of infix operators.
        let result_defixed = SDefix.defix SDefix.defaultFixTable mm_fresh 
        mm_defixed      <- case result_defixed of
                            Left err        -> throwE [ErrorLoad [err]]
                            Right mm'       -> return mm'

        liftIO $ pipeSink (renderIndent $ ppr mm_defixed) sinkDefix


        -- Expand missing quantifiers in signatures.
        let sp          = SP.SourcePos "<top level>" 1 1
        let mm_expand   = SExpand.expandModule sp mm_defixed
        liftIO $ pipeSink (renderIndent $ ppr mm_expand)  sinkExpand


        -- Desugar guards and patterns to match expressions.
        let mm_guards   = SGuards.evalState   (Text.pack "g")
                        $ SGuards.desugarModule mm_expand
        liftIO $ pipeSink (renderIndent $ ppr mm_guards)  sinkGuards


        -- Desugar match expressions to case expressions.
        let mm_match    = SMatches.evalState  (Text.pack "m")
                        $ SMatches.desugarModule mm_guards
        liftIO $ pipeSink (renderIndent $ ppr mm_match)   sinkMatches

                
        -- Prepare for conversion to core.
        let mm_prep     = SPrep.evalState     (Text.pack "p")
                        $ SPrep.desugarModule mm_match
        liftIO $ pipeSink (renderIndent $ ppr mm_prep)    sinkPrep

        return mm_prep


-------------------------------------------------------------------------------
-- | Lower desugared source tetra code to core tetra.
lowerSourceTetra 
        :: Sink                 -- ^ Sink after conversion to core.
        -> Sink                 -- ^ Sink after spreading.
        -> Store
        -> S.Module S.Source
        -> ExceptT [Error] IO (C.Module BP.SourcePos CE.Name)

lowerSourceTetra sinkCore sinkSpread store mm
 = do   
        -- Lower source tetra to core tetra.
        let sp          = SP.SourcePos "<top level>" 1 1
        mm_core         <- case SConvert.coreOfSourceModule sp mm of
                            Left err    -> throwE [ErrorLoad [err]]
                            Right mm'   -> return mm'

        liftIO $ pipeSink (renderIndent $ ppr mm_core) sinkCore


        -- Resolve which module imported names are from, 
        -- and attach arity information to the import statements.
        result_resolve  <- liftIO $ B.resolveNamesInModule 
                                        CE.primKindEnv CE.primTypeEnv
                                        store mm_core

        mm_resolve      <- case result_resolve of
                            Left err    -> throwE [ErrorLoad [err]]
                            Right mm'   -> return mm'

        -- TODO: dump resolved

        -- Spread types of data constructors into uses.
        let mm_spread   = C.spreadX CE.primKindEnv CE.primTypeEnv mm_resolve
        liftIO $ pipeSink (renderIndent $ ppr mm_spread) sinkSpread

        return mm_spread


