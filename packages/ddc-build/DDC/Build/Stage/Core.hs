
module DDC.Build.Stage.Core
        ( coreLoad
        , coreCheck
        , coreReCheck
        , coreResolve
        , coreSimplify)
where
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.DeepSeq

import DDC.Data.Pretty
import DDC.Data.Name

import qualified DDC.Data.SourcePos                     as SP

import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Load                          as C
import qualified DDC.Core.Exp                           as C
import qualified DDC.Core.Simplifier                    as C

import qualified DDC.Core.Transform.Reannotate          as CReannotate
import qualified DDC.Core.Transform.Resolve             as CResolve



---------------------------------------------------------------------------------------------------
-- | Load a core module from text.
coreLoad
        :: (Ord n, Show n, Pretty n, Pretty (err (C.AnTEC SP.SourcePos n)))
        => String                       -- ^ Name of compiler stage.
        -> C.Fragment n err             -- ^ Language fragment to check.
        -> C.Mode n                     -- ^ Checker mode.
        -> String                       -- ^ Name of source file.
        -> Int                          -- ^ Line of source file.
        -> B.Sink                       -- ^ Sink for checker trace.
        -> String                       -- ^ Textual core code.
        -> ExceptT [B.Error] IO (C.Module (C.AnTEC SP.SourcePos n) n)

coreLoad !_stage !fragment !mode !srcName !srcLine !sinkTrace !str
 = do   
        -- Lex the module to tokens, then parse and type-check it.
        let (eError, mTrace) 
                = C.loadModuleFromTokens fragment srcName mode
                $ C.fragmentLexModule    fragment srcName srcLine str 

        -- Dump the type checker trace.
        _       <- case mTrace of
                        Nothing       
                         -> return []

                        Just (C.CheckTrace doc)
                         -> liftIO $ B.pipeSink (renderIndent doc) sinkTrace

        -- Check if there were errors on load.
        case eError of
         Left err
          -> throwE [B.ErrorLoad err]

         Right mm
          -> return mm


---------------------------------------------------------------------------------------------------
-- | Type check a module.
coreCheck
        :: ( Pretty a, Show a
           , Pretty (err (C.AnTEC a n))
           , Ord n, Show n, Pretty n)
        => String                       -- ^ Name of compiler stage.
        -> C.Fragment n err             -- ^ Language fragment to check.
        -> C.Mode n                     -- ^ Checker mode.
        -> B.Sink                       -- ^ Sink for checker trace.
        -> B.Sink                       -- ^ Sink for checked core code.
        -> C.Module a n                 -- ^ Core module to check.
        -> ExceptT [B.Error] IO (C.Module (C.AnTEC a n) n)

coreCheck !stage !fragment !mode !sinkTrace !sinkChecked !mm
 = {-# SCC "coreCheck" #-}
   do
        let profile  = C.fragmentProfile fragment

        -- Type check the module with the generic core type checker.
        mm_checked      
         <- case C.checkModule (C.configOfProfile profile) mm mode of
                (Left err,  C.CheckTrace doc) 
                 -> do  liftIO $  B.pipeSink (renderIndent doc) sinkTrace
                        throwE $ [B.ErrorLint stage "PipeCoreCheck/Check" err]
                        
                (Right mm', C.CheckTrace doc) 
                 -> do  liftIO $ B.pipeSink (renderIndent doc) sinkTrace
                        return mm'


        -- Check that the module compiles with the language profile.
        mm_complies
         <- case C.complies profile mm_checked of
                Just err -> throwE [B.ErrorLint stage "PipeCoreCheck/Complies" err]
                Nothing  -> return mm_checked


        -- Check that the module satisfies fragment specific checks.
        mm_fragment
         <- case C.fragmentCheckModule fragment mm_complies of
                Just err -> throwE [B.ErrorLint stage "PipeCoreCheck/Fragment" err]
                Nothing  -> return mm_complies

        liftIO $ B.pipeSink (renderIndent $ ppr mm_fragment) sinkChecked

        return mm_fragment


---------------------------------------------------------------------------------------------------
-- | Re-check a core module, replacing existing type annotations.
coreReCheck
        :: ( Pretty a, Show a
           , Pretty (err (C.AnTEC a n))
           , Ord n, Show n, Pretty n)
        => String                       -- ^ Name of compiler stage.
        -> C.Fragment n err             -- ^ Language fragment to check.
        -> C.Mode n                     -- ^ Checker mode.
        -> B.Sink                       -- ^ Sink for checker trace.
        -> B.Sink                       -- ^ Sink for checked core code.
        -> C.Module (C.AnTEC a n) n     -- ^ Core module to check.
        -> ExceptT [B.Error] IO (C.Module (C.AnTEC a n) n)

coreReCheck !stage !fragment !mode !sinkTrace !sinkChecked !mm
 = {-# SCC "coreReCheck" #-}
   do
        let mm_reannot  = CReannotate.reannotate C.annotTail mm
        coreCheck stage fragment mode sinkTrace sinkChecked mm_reannot


---------------------------------------------------------------------------------------------------
-- | Resolve elaborations in a core module.
coreResolve
        :: (Pretty a, Eq n, Ord n, Show n, Pretty n)
        => String                       -- ^ Name of compiler stage.
        -> C.Fragment n arr             -- ^ Language fragment to use.
        -> IO [(n, C.ImportValue n (C.Type n))]        
                                        -- ^ Top level env from other modules.
        -> C.Module a n
        -> ExceptT [B.Error] IO (C.Module a n)

coreResolve !stage !fragment !makeNtsTop !mm
 = {-# SCC "coreResolve" #-}
   do   
        ntsTop  <- liftIO $ makeNtsTop

        res     <- liftIO $ CResolve.resolveModule 
                        (C.fragmentProfile fragment) 
                        ntsTop mm
                
        case res of
         Left  err  -> throwE [B.ErrorLint stage "PipeCoreResolve" err]
         Right mm'  -> return mm'


---------------------------------------------------------------------------------------------------
-- | Simplify a core module.
coreSimplify
        :: ( Pretty a, Show a,
             CompoundName n, NFData n, Ord n, Show n, Pretty n)
        => C.Fragment n err
        -> s
        -> C.Simplifier s a n
        -> C.Module a n
        -> ExceptT [B.Error] IO (C.Module () n)

coreSimplify fragment nameZero simpl mm
 = {-# SCC "coreSimplify" #-}
   do   
        let profile     = C.fragmentProfile fragment
        let primKindEnv = C.profilePrimKinds      profile
        let primTypeEnv = C.profilePrimTypes      profile

        let !mm'        = C.result . flip evalState nameZero
                        $ C.applySimplifier profile primKindEnv primTypeEnv simpl mm

        let !mm2        = CReannotate.reannotate (const ()) mm'

        -- NOTE: It is helpful to deepseq here so that we release 
        --       references to the unsimplified version of the code.
        --       Because we've just applied reannotate, we also
        --       release type annotations on the expression tree.
        return $ (mm2 `deepseq` mm2)

