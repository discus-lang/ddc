
module DDC.Build.Pipeline.Llvm
        ( PipeLlvm(..)
        , pipeLlvm)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Builder
import DDC.Llvm.Pretty                          ()
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Llvm.Syntax                as Llvm
import System.Directory


-- | Process an LLVM module.
data PipeLlvm
        = PipeLlvmPrint     Sink

        | PipeLlvmCompile   
        { pipeBuilder           :: Builder
        , pipeFileLlvm          :: FilePath
        , pipeFileAsm           :: FilePath
        , pipeFileObject        :: FilePath
        , pipeFileExe           :: Maybe FilePath 
        , pipeKeepLlvmFiles     :: Bool 
        , pipeKeepAsmFiles      :: Bool }
        deriving (Show)


-- | Process an LLVM module.
--
--   Returns empty list on success.
pipeLlvm 
        :: Llvm.Module 
        -> PipeLlvm 
        -> IO [Error]

pipeLlvm !mm !pp
 = case pp of
        PipeLlvmPrint !sink
         -> {-# SCC "PipeLlvmPrint" #-} 
            pipeSink (renderIndent $ ppr mm) sink

        PipeLlvmCompile 
                !builder !llPath !sPath !oPath !mExePath
                !keepLlvmFiles !keepAsmFiles
         -> {-# SCC "PipeLlvmCompile" #-}
            do  -- Write out the LLVM source file.
                let llSrc       = renderIndent $ ppr mm
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
                buildLlc builder llPath sPath

                -- Assemble .s file into .o file
                buildAs builder  sPath  oPath

                -- Link .o file into an executable if we were asked for one.      
                (case mExePath of
                  Nothing 
                   -> return ()

                  Just exePath
                   -> do buildLdExe builder oPath exePath
                         return ())

                -- Remove LLVM IR files if we weren't asked for them.
                when (not keepLlvmFiles)
                 $ removeFile llPath

                -- Remove Asm IR files if we weren't asked for them.
                when (not keepAsmFiles)
                 $ removeFile sPath

                return []

