
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
import DDCI.Core.Mode
import DDCI.Core.State
import DDCI.Core.Language
import DDCI.Core.Build.Builder
import DDC.Core.Simplifier
import DDC.Base.Pretty
import qualified DDC.Core.Load                  as Core
import qualified DDC.Core.Module                as Core
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Llvm.Platform         as Llvm
import qualified DDC.Core.Sea.Output.Convert    as Output
import qualified DDC.Core.Sea.Output.Name       as Output
import qualified DDC.Llvm.Module                as Llvm
import Control.Monad

-- Error ----------------------------------------------------------------------
data Error
        = ErrorSeaLoad    (Core.Error Output.Name)
        | ErrorSeaConvert (Output.Error ())
        | forall err. Pretty err => ErrorLoad err


instance Pretty Error where
 ppr err
  = case err of
        ErrorSeaLoad err'
         -> vcat [ text "Type error when loading Sea module."
                 , indent 2 (ppr err') ]

        ErrorSeaConvert err'
         -> vcat [ text "Fragment violation when converting Sea module to C code."
                 , indent 2 (ppr err') ]

        ErrorLoad err'
         -> vcat [ text "Error loading module"
                 , indent 2 (ppr err') ]

-- PipeSource -----------------------------------------------------------------
data PipeTextModule
        = PipeTextModuleOutput   Sink
        | PipeTextModuleLoadCore Language [PipeCoreModule]
        | PipeTextModuleLoadSea  [PipeSeaModule]
        deriving (Show)

pipeTextModule
        :: Source
        -> String
        -> PipeTextModule
        -> IO [Error]

pipeTextModule source str pp
 = case pp of
        PipeTextModuleOutput sink
         -> pipeSink str sink

        PipeTextModuleLoadCore language pipes
         | Language (Fragment profile lexString _ _)     <- language
         -> let sourceName      = nameOfSource source
                toks            = lexString source str

            in case Core.loadModule profile sourceName toks of
                 Left err -> return $ [ErrorLoad err]
                 Right mm -> liftM concat $ mapM (pipeCoreModule mm) pipes

        PipeTextModuleLoadSea pipes
         | Fragment profile lexString _ _       <- fragmentSea
         -> let sourceName      = nameOfSource source
                toks            = lexString source str

            in case Core.loadModule profile sourceName toks of
                 Left err -> return $ [ErrorLoad err]
                 Right mm -> liftM concat $ mapM (pipeSeaModule mm) pipes


-- PipeCoreModule -------------------------------------------------------------
data PipeCoreModule
        = PipeCoreModuleOutput    Sink
        | PipeCoreModuleSimplify  Simplifier [PipeCoreModule]
        | PipeCoreModuleToSea     [PipeSeaModule]
        deriving (Show)

pipeCoreModule
        :: (Eq n, Ord n, Pretty n)
        => Core.Module () n
        -> PipeCoreModule
        -> IO [Error]

pipeCoreModule mm pp
 = case pp of
        PipeCoreModuleOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeCoreModuleSimplify simpl pipes
         -> let mm'     = applySimplifier simpl mm 
            in  liftM concat $ mapM (pipeCoreModule mm') pipes

        _       -> error "pipeCoreModule: finish me"


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
        :: Core.Module () Output.Name 
        -> PipeSeaModule 
        -> IO [Error]

pipeSeaModule mm pp
 = case pp of
        PipeSeaModuleOutput _sink
         -> error "need module pretty printer"

        PipeSeaModulePrint withPrelude sink
         -> case Output.convertModule mm of
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
        { pipeBuilder           :: Builder
        , pipeFileLlvm          :: FilePath
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
        PipeLlvmModulePrint sink
         ->     pipeSink (renderIndent $ ppr mm) sink

        PipeLlvmModuleCompile builder llPath sPath oPath exePath
         -> do  -- Write out the LLVM source file.
                let llSrc       = renderIndent $ ppr mm
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
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

