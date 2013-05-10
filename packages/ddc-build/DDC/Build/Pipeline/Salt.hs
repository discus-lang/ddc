{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Salt
        ( PipeSalt (..)
        , pipeSalt)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Llvm
import DDC.Build.Builder
import DDC.Base.Pretty
import DDC.Llvm.Pretty                          ()
import DDC.Core.Check                           (AnTEC)
import qualified DDC.Core.Transform.Reannotate  as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Salt.Transfer         as Salt
import qualified DDC.Core.Salt.Platform         as Salt
import qualified DDC.Core.Salt                  as Salt
import Control.Monad
import Control.DeepSeq
import System.Directory


-- | Process a Core Salt module.
data PipeSalt a where
        -- Plumb the module on without doing anything to it.
        PipeSaltId
                :: ![PipeSalt a]
                -> PipeSalt a

        -- Output the module in core language syntax.
        PipeSaltOutput 
                :: !Sink
                -> PipeSalt a

        -- Insert control-transfer primops.
        --      This needs to be done before we convert the module to C or LLVM.
        PipeSaltTransfer
                :: ![PipeSalt (AnTEC a Salt.Name)]
                -> PipeSalt (AnTEC a Salt.Name)

        -- Print the module as a C source code.
        PipeSaltPrint      
                :: !Bool                 -- With C prelude.
                -> !Salt.Platform        -- Target platform specification
                -> !Sink 
                -> PipeSalt a

        -- Convert the module to LLVM.
        PipeSaltToLlvm
                :: !Salt.Platform 
                -> ![PipeLlvm]
                -> PipeSalt a

        -- Compile the module via C source code.
        PipeSaltCompile
                :: !Salt.Platform        --  Target platform specification
                -> !Builder              --  Builder to use.
                -> !FilePath             --  Intermediate C file.
                -> !FilePath             --  Object file.
                -> !(Maybe FilePath)     --  Link into this exe file
                -> !Bool                 --  Keep intermediate .c files
                -> PipeSalt a

deriving instance Show a => Show (PipeSalt a)


-- | Process a Core Salt module.
--  
--   Returns empty list on success.
pipeSalt  :: (Show a, Pretty a, NFData a)
          => C.Module a Salt.Name
          -> PipeSalt a
          -> IO [Error]

pipeSalt !mm !pp
 = case pp of
        PipeSaltId !pipes
         -> {-# SCC "PipeSaltId" #-}
            liftM concat $ mapM (pipeSalt mm) pipes

        PipeSaltOutput !sink
         -> {-# SCC "PipeSaltOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeSaltTransfer !pipes
         -> {-# SCC "PipeSaltTransfer" #-}
            case Salt.transferModule mm of
                Left err        -> return [ErrorSaltConvert err]
                Right mm'       -> liftM concat $ mapM (pipeSalt mm') pipes

        PipeSaltPrint !withPrelude !platform !sink
         -> {-# SCC "PipeSaltPrint" #-}
            case Salt.seaOfSaltModule withPrelude platform mm of
                Left  err 
                 -> return $ [ErrorSaltConvert err]

                Right doc 
                 -> pipeSink (renderIndent doc)  sink

        PipeSaltToLlvm !platform !more
         -> {-# SCC "PipeSaltToLlvm" #-}
            do  let !mm_cut  = C.reannotate (const ()) mm
                let !mm'     = Llvm.convertModule platform mm_cut 
                results <- mapM (pipeLlvm mm') more
                return  $ concat results

        PipeSaltCompile 
                !platform !builder !cPath !oPath !mExePath
                !keepSeaFiles
         -> {-# SCC "PipeSaltCompile" #-}
            case Salt.seaOfSaltModule True platform mm of
             Left errs
              -> error $ show errs

             Right cDoc
              -> do let cSrc        = renderIndent cDoc
                    writeFile cPath cSrc

                    -- Compile C source file into .o file.
                    buildCC  builder cPath oPath

                    -- Link .o file into an executable if we were asked for one.      
                    (case mExePath of
                      Nothing -> return ()
                      Just exePath
                       -> do buildLdExe builder oPath exePath
                             return ())

                    -- Remove intermediate .c files if we weren't asked for them.
                    when (not keepSeaFiles)
                     $ removeFile cPath

                    return []
