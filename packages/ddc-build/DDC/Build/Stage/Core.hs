
module DDC.Build.Stage.Core
        ( coreLoad
        , coreCheck )
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import DDC.Data.Pretty

import qualified DDC.Data.SourcePos                     as SP

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Load                          as C

import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B


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
        -> C.Module a n                 -- ^ Core module to check.
        -> ExceptT [B.Error] IO (C.Module (C.AnTEC a n) n)

coreCheck !stage !fragment !mode !sinkTrace !mm
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

        return mm_fragment


