
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
import DDCI.Core.Language
import DDCI.Core.Build.Builder
import DDC.Core.Simplifier
import DDC.Core.Language.Profile
import DDC.Base.Pretty
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Load                  as CL
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Llvm.Platform         as Llvm
import qualified DDC.Core.Sea.Output.Convert    as Output
import qualified DDC.Core.Sea.Output.Name       as Output
import qualified DDC.Llvm.Module                as Llvm
import Control.Monad

-- Error ----------------------------------------------------------------------
data Error
        = ErrorSeaLoad    (CL.Error Output.Name)
        | ErrorSeaConvert (Output.Error ())

        -- | Error when loading a module.
        --   Blame it on the user.
        | forall err. Pretty err => ErrorLoad  err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint err


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

        ErrorLint err'
         -> vcat [ text "Error in transformed module."
                 , indent 2 (ppr err') ]


-- PipeSource -----------------------------------------------------------------
-- | Process program text.
data PipeTextModule n (err :: * -> *) where
  -- | Output the current module text.
  PipeTextModuleOutput 
        :: Sink
        -> PipeTextModule n err

  -- | Parse and type check the text as a core module.
  PipeTextModuleLoadCore 
        :: (Ord n, Show n, Pretty n)
        => Fragment n err
        -> [PipeCoreModule n]
        -> PipeTextModule n err

deriving instance Show (PipeTextModule n err)


-- | Text module pipeline.
pipeTextModule
        :: Source
        -> String
        -> PipeTextModule n err
        -> IO [Error]

pipeTextModule source str pp
 = case pp of
        PipeTextModuleOutput sink
         -> pipeSink str sink

        PipeTextModuleLoadCore fragment pipes
         | (Fragment profile lexString _ _)     <- fragment
         -> let sourceName      = nameOfSource source
                toks            = lexString source str

            in case CL.loadModule profile sourceName toks of
                 Left err -> return $ [ErrorLoad err]
                 Right mm -> liftM concat $ mapM (pipeCoreModule mm) pipes


-- PipeCoreModule -------------------------------------------------------------
-- | Process a core module.
data PipeCoreModule n where
  -- | Output the module in core language syntax.
  PipeCoreModuleOutput    
        :: Sink 
        -> PipeCoreModule n

  -- | Type check the module.
  PipeCoreModuleCheck      
        :: Fragment n err
        -> [PipeCoreModule n]
        -> PipeCoreModule n

  -- | Apply a simplifier to the module.
  PipeCoreModuleSimplify  
        :: Simplifier 
        -> [PipeCoreModule n] 
        -> PipeCoreModule n

  -- | Specialised processing for modules in the Core Sea fragment.
  PipeCoreModuleAsSea
        :: [PipeSeaModule] 
        -> PipeCoreModule Output.Name

deriving instance Show (PipeCoreModule n)


-- | Core module pipeline.
pipeCoreModule
        :: (Eq n, Ord n, Show n, Pretty n)
        => C.Module () n
        -> PipeCoreModule n
        -> IO [Error]

pipeCoreModule mm pp
 = case pp of
        PipeCoreModuleOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeCoreModuleCheck fragment pipes
         -> let profile         = fragmentProfile fragment
                primDataDefs    = profilePrimDataDefs   profile
                primKindEnv     = profilePrimKinds      profile
                primTypeEnv     = profilePrimTypes      profile
            in  case C.checkModule primDataDefs primKindEnv primTypeEnv mm of
                  Left err  -> return $ [ErrorLint err]
                  Right mm' -> liftM concat $ mapM (pipeCoreModule mm') pipes

        PipeCoreModuleSimplify simpl pipes
         -> let mm'     = applySimplifier simpl mm 
            in  liftM concat $ mapM (pipeCoreModule mm') pipes

        PipeCoreModuleAsSea pipes
         -> liftM concat $ mapM (pipeSeaModule mm) pipes


-- PipeSeaModule --------------------------------------------------------------
-- | Process a Core Sea module.
data PipeSeaModule
        -- | Output the module in core language syntax.
        = PipeSeaModuleOutput     Sink

        -- | Print the module as a C source code.
        | PipeSeaModulePrint      
        { pipeWithSeaPrelude    :: Bool
        , pipeModuleSink        :: Sink }

        -- | Compile the module into an object file.
        | PipeSeaModuleCompile    FilePath

        -- | Convert the module to LLVM.
        | PipeSeaModuleToLlvm     [PipeLlvmModule]
        deriving (Show)


-- | Process a Core Sea module.
pipeSeaModule 
        :: C.Module () Output.Name 
        -> PipeSeaModule 
        -> IO [Error]

pipeSeaModule mm pp
 = case pp of
        PipeSeaModuleOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

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

        PipeSeaModuleToLlvm more
         -> do  let mm'     =  Llvm.convertModule Llvm.platform32 mm
                results <- mapM (pipeLlvmModule mm') more
                return  $ concat results


-- PipeLlvmModule -------------------------------------------------------------
-- | Process an LLVM module.
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
-- | What to do with program text.
data Sink
        -- | Drop it on the floor.
        = SinkDiscard

        -- | Emit it to stdout.
        | SinkStdout

        -- | Write it to this file.
        | SinkFile FilePath
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

