
module DDCI.Core.Pipeline.Module
        ( Error(..)

        , PipeTextModule        (..)
        , pipeTextModule

        , PipeCoreModule        (..)

        , PipeSeaModule         (..)
        , pipeSeaModule

        , PipeLlvmModule        (..)
        , pipeLlvmModule

        , Sink                  (..)
        , pipeSink)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Transform
import DDC.Base.Pretty
import qualified DDC.Core.Load                  as Core
import qualified DDC.Core.Module                as Core
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Llvm.Platform         as Llvm
import qualified DDC.Core.Sea.Output.Convert    as Sea
import qualified DDC.Core.Sea.Output.Profile    as Sea
import qualified DDC.Core.Sea.Output.Name       as Sea
import qualified DDC.Llvm.Module                as Llvm
import Control.Monad

-- Error ----------------------------------------------------------------------
data Error
        = ErrorSeaLoad    (Core.Error Sea.Name)
        | ErrorSeaConvert (Sea.Error ())
        deriving (Show)

instance Pretty Error where
 ppr err
  = case err of
        ErrorSeaLoad err'
         -> vcat [ text "Type error when loading Sea module."
                 , indent 2 (ppr err') ]

        ErrorSeaConvert err'
         -> vcat [ text "Fragment violation when converting Sea module to C code."
                 , indent 2 (ppr err') ]


-- PipeSource -----------------------------------------------------------------
data PipeTextModule
        = PipeTextModuleOutput   Sink
        | PipeTextModuleLoadCore [PipeCoreModule]
        | PipeTextModuleLoadSea  [PipeSeaModule]
        deriving (Show)

pipeTextModule
        :: Int
        -> String
        -> PipeTextModule
        -> IO [Error]

pipeTextModule lineStart str pp
 = case pp of
        PipeTextModuleOutput sink
         -> pipeSink str sink

        PipeTextModuleLoadCore _
         -> error "finish me"

        PipeTextModuleLoadSea pipes
         -> let toks    = Sea.lexString lineStart str
            in  case Core.loadModule Sea.outputProfile "<interactive>" toks of
                 Left err -> return [ErrorSeaLoad err]
                 Right mm -> liftM concat $ mapM (pipeSeaModule mm) pipes


-- PipeCoreModule -------------------------------------------------------------
data PipeCoreModule
        = PipeCoreModuleOutput    Sink
        | PipeCoreModuleTransform Transform [PipeCoreModule]
        | PipeCoreModuleToSea     [PipeSeaModule]
        deriving (Show)


-- PipeSeaModule --------------------------------------------------------------
data PipeSeaModule
        -- | Output the module in core language syntax.
        = PipeSeaModuleOutput     Sink

        -- | Print the module as a C source code.
        | PipeSeaModulePrint      
        { pipeWithSeaPrelude    :: Bool
        , pipeModuleSink        :: Sink }

        -- | Compile the module into an object file.
        | PipeSeaModuleCompile    FilePath

        -- | Type check the module.
        | PipeSeaModuleCheck      [PipeSeaModule]

        -- | Convert the module to LLVM.
        | PipeSeaModuleToLlvm     [PipeLlvmModule]
        deriving (Show)


-- | Process a Core Sea module.
pipeSeaModule 
        :: Core.Module () Sea.Name 
        -> PipeSeaModule 
        -> IO [Error]

pipeSeaModule mm pp
 = case pp of
        PipeSeaModuleOutput _sink
         -> error "need module pretty printer"

        PipeSeaModulePrint withPrelude sink
         -> case Sea.convertModule mm of
                Left  err 
                 ->     return $ [ErrorSeaConvert err]

                Right doc 
                 | withPrelude
                 -> do  let doc' = vcat
                                [ text "#include <Disciple.h>"
                                , text "#include <Primitive.h>" 
                                , line 
                                , doc ]
                        pipeSink (renderIndent doc') sink

                 | otherwise
                 -> pipeSink (renderIndent doc)  sink

        PipeSeaModuleCompile _
         -> error "finish me"

        PipeSeaModuleCheck _
         -> error "finish me"

        PipeSeaModuleToLlvm more
         -> do  let mm'     =  Llvm.convertModule Llvm.platform32 mm
                results <- mapM (pipeLlvmModule mm') more
                return  $ concat results


-- PipeLlvmModule -------------------------------------------------------------
data PipeLlvmModule
        = PipeLlvmModulePrint     Sink

        | PipeLlvmModuleCompile   
        { pipeFileLlvm          :: FilePath
        , pipeFileAsm           :: FilePath
        , pipeFileObject        :: FilePath
        , pipeFileExe           :: FilePath }
        deriving (Show)


-- | Process an LLVM module.
pipeLlvmModule 
        :: Llvm.Module 
        -> PipeLlvmModule 
        -> IO [Error]

pipeLlvmModule mm pp
 = case pp of
        PipeLlvmModulePrint _
         -> error "need module pretty printer"

        PipeLlvmModuleCompile llPath sPath oPath exePath
         -> do  -- Write out the LLVM source file.
                let llSrc       = renderIndent $ ppr mm
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
                let builder     = builder_I386_Darwin           -- add this to the pipeline structure
                buildLlc builder llPath sPath

                -- Assemble .s file into .o file
                buildAs builder  sPath  oPath

                -- Link .o file into an executable.
                buildLdExe builder oPath exePath

                return []


-- Target ---------------------------------------------------------------------
data Sink
        = SinkDiscard
        | SinkStdout
        | SinkFile            FilePath
        deriving (Show)


pipeSink :: String -> Sink -> IO [Error]
pipeSink str tg
 = case tg of
        SinkDiscard
         -> do  return []

        SinkStdout
         -> do  putStrLn str
                return []

        SinkFile path
         -> do  writeFile path str
                return []

