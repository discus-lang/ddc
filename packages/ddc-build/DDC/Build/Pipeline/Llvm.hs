
module DDC.Build.Pipeline.Llvm
        ( PipeLlvm(..)
        , pipeLlvm)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Build.Builder              as Build
import qualified DDC.Llvm.Pretty                as Llvm
import qualified DDC.Llvm.Syntax                as Llvm
import System.Directory


-- | Process an LLVM module.
data PipeLlvm
        = PipeLlvmPrint     Sink

        | PipeLlvmCompile   
        { pipeBuilder           :: Build.Builder
        , pipeFileLlvm          :: FilePath
        , pipeFileAsm           :: FilePath
        , pipeFileObject        :: FilePath
        , pipeFileExe           :: Maybe FilePath
        , pipeLinkOtherObjects  :: [FilePath]
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
                !builder !llPath !sPath !oPath !mExePath !osLinkOther
                !keepLlvmFiles !keepAsmFiles
         -> {-# SCC "PipeLlvmCompile" #-}
            do  
                -- LLVM config.
                let llConfig    = Llvm.configOfVersion
                                $ Just $ Build.buildLlvmVersion builder

                -- Pretty printer mode to use for the current LLVM version.
                let llMode      = Llvm.prettyModeModuleOfConfig llConfig

                -- Write out the LLVM source file.
                let llSrc       = renderIndent $ pprModePrec llMode (0 :: Int) mm
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
                Build.buildLlc builder llPath sPath

                -- Assemble .s file into .o file
                Build.buildAs builder  sPath  oPath

                -- Link .o file into an executable if we were asked for one.      
                (case mExePath of
                  Nothing 
                   -> return ()

                  Just exePath
                   -> do Build.buildLdExe builder (oPath : osLinkOther) exePath
                         return ())

                -- Remove LLVM IR files if we weren't asked for them.
                when (not keepLlvmFiles)
                 $ removeFile llPath

                -- Remove Asm IR files if we weren't asked for them.
                when (not keepAsmFiles)
                 $ removeFile sPath

                return []

