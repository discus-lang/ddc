{-# LANGUAGE GADTs #-}
module DDCI.Core.Pipeline.Module
        ( -- * Things that can go wrong
          Error(..)

          -- * Processing text source code
        , PipeText        (..)
        , pipeText

          -- * Processing core modules
        , PipeCore        (..)
        , pipeCore

          -- * Processing Core Lite modules
        , PipeLite        (..)
        , pipeLite

          -- * Processing Core Brine modules
        , PipeBrine       (..)
        , pipeBrine

          -- * Processing LLVM modules
        , PipeLlvm        (..)
        , pipeLlvm

          -- * Emitting output
        , Sink                  (..)
        , pipeSink)
where
import DDCI.Core.Mode
import DDCI.Core.Language
import DDCI.Core.Build.Builder
import DDC.Core.Simplifier
import DDC.Core.Fragment.Profile
import DDC.Core.Collect
import DDC.Base.Pretty
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Load                  as CL
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Llvm.Platform         as Llvm
import qualified DDC.Core.Brine.Lite            as Lite
import qualified DDC.Core.Brine.Output          as Output
import qualified DDC.Llvm.Module                as Llvm
import qualified DDC.Type.Env                   as Env
import qualified Control.Monad.State.Strict     as S
import Control.Monad

-- Error ----------------------------------------------------------------------
data Error
        = ErrorBrineLoad    (CL.Error Output.Name)
        | ErrorBrineConvert (Output.Error ())

        -- | Error when loading a module.
        --   Blame it on the user.
        | forall err. Pretty err => ErrorLoad  err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint err


instance Pretty Error where
 ppr err
  = case err of
        ErrorBrineLoad err'
         -> vcat [ text "Type error when loading Brine module."
                 , indent 2 (ppr err') ]

        ErrorBrineConvert err'
         -> vcat [ text "Fragment violation when converting Brine module to C code."
                 , indent 2 (ppr err') ]

        ErrorLoad err'
         -> vcat [ text "Error loading module"
                 , indent 2 (ppr err') ]

        ErrorLint err'
         -> vcat [ text "Error in transformed module."
                 , indent 2 (ppr err') ]


-- PipeSource -----------------------------------------------------------------
-- | Process program text.
data PipeText n (err :: * -> *) where
  PipeTextOutput 
        :: Sink
        -> PipeText n err

  PipeTextLoadCore 
        :: (Ord n, Show n, Pretty n)
        => Fragment n err
        -> [PipeCore n]
        -> PipeText n err

deriving instance Show (PipeText n err)


-- | Text module pipeline.
pipeText
        :: Source
        -> String
        -> PipeText n err
        -> IO [Error]

pipeText source str pp
 = case pp of
        PipeTextOutput sink
         -> pipeSink str sink

        PipeTextLoadCore frag pipes
         -> let sourceName      = nameOfSource source
                toks            = fragmentLexModule frag source str
            in case CL.loadModule (fragmentProfile frag) sourceName toks of
                 Left err -> return $ [ErrorLoad err]
                 Right mm -> liftM concat $ mapM (pipeCore mm) pipes


-- PipeCoreModule -------------------------------------------------------------
-- | Process a core module.
data PipeCore n where
  -- Output a module to console or file.
  PipeCoreOutput    
        :: Sink 
        -> PipeCore n

  -- Type check a module.
  PipeCoreCheck      
        :: Fragment n err
        -> [PipeCore n]
        -> PipeCore n

  -- Apply a simplifier to a module.
  PipeCoreSimplify  
        :: Fragment n err
        -> Simplifier 
        -> [PipeCore n] 
        -> PipeCore n

  -- Treat a module as belonging to the Core Lite fragment from now on.
  PipeCoreAsLite
        :: [PipeLite]
        -> PipeCore Lite.Name

  -- Treat a module as beloning to the Core Brine fragment from now on.
  PipeCoreAsBrine
        :: [PipeBrine] 
        -> PipeCore Output.Name

deriving instance Show (PipeCore n)


-- | Core module pipeline.
pipeCore
        :: (Eq n, Ord n, Show n, Pretty n)
        => C.Module () n
        -> PipeCore n
        -> IO [Error]

pipeCore mm pp
 = case pp of
        PipeCoreOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeCoreCheck fragment pipes
         -> let profile         = fragmentProfile fragment
                primDataDefs    = profilePrimDataDefs   profile
                primKindEnv     = profilePrimKinds      profile
                primTypeEnv     = profilePrimTypes      profile
            in  case C.checkModule primDataDefs primKindEnv primTypeEnv mm of
                  Left err  -> return $ [ErrorLint err]
                  Right mm' -> liftM concat $ mapM (pipeCore mm') pipes

        PipeCoreSimplify frag simpl pipes
         | Fragment _ _ _ _ _ makeNamifierT makeNamifierX nameZero <- frag
         -> let 
                -- Collect up names used as binders,
                -- We pass these to the namifiers so they know not to
                -- return these names when asked for a fresh variable.
                (tbinds, xbinds) = collectBinds mm
                kenv    = Env.fromList tbinds
                tenv    = Env.fromList xbinds
                mm'     = flip S.evalState nameZero
                        $ applySimplifier 
                                simpl 
                                (makeNamifierT kenv)
                                (makeNamifierX tenv)
                                mm 

            in  liftM concat $ mapM (pipeCore mm') pipes

        PipeCoreAsLite pipes
         -> liftM concat $ mapM (pipeLite mm) pipes

        PipeCoreAsBrine pipes
         -> liftM concat $ mapM (pipeBrine mm) pipes


-- PipeLiteModule -------------------------------------------------------------
-- | Process a Core Lite module.
data PipeLite
        -- | Output the module in core language syntax.
        = PipeLiteOutput    Sink

        -- | Convert the module to the Core Sea Fragment.
        | PipeLiteToBrine     [PipeCore Output.Name]
        deriving Show

pipeLite :: C.Module () Lite.Name
         -> PipeLite
         -> IO [Error]

pipeLite mm pp
 = case pp of
        PipeLiteOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeLiteToBrine pipes
         -> do  let mm'     = Lite.toBrine mm
                results     <- mapM (pipeCore mm') pipes
                return      $ concat results


-- PipeSeaModule --------------------------------------------------------------
-- | Process a Core Sea module.
data PipeBrine
        -- | Output the module in core language syntax.
        = PipeBrineOutput     Sink

        -- | Print the module as a C source code.
        | PipeBrinePrint      
        { pipeWithBrinePrelude  :: Bool
        , pipeModuleSink        :: Sink }

        -- | Convert the module to LLVM.
        | PipeBrineToLlvm        Llvm.Platform [PipeLlvm]
        deriving (Show)


-- | Process a Core Brine module.
pipeBrine :: C.Module () Output.Name 
          -> PipeBrine
          -> IO [Error]

pipeBrine mm pp
 = case pp of
        PipeBrineOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeBrinePrint withPrelude sink
         -> case Output.convertModule mm of
                Left  err 
                 ->     return $ [ErrorBrineConvert err]

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

        PipeBrineToLlvm platform more
         -> do  let mm'     =  Llvm.convertModule platform mm
                results <- mapM (pipeLlvm mm') more
                return  $ concat results


-- PipeLlvmModule -------------------------------------------------------------
-- | Process an LLVM module.
data PipeLlvm
        = PipeLlvmPrint     Sink

        | PipeLlvmCompile   
        { pipeBuilder           :: Builder
        , pipeFileLlvm          :: FilePath
        , pipeFileAsm           :: FilePath
        , pipeFileObject        :: FilePath
        , pipeFileExe           :: Maybe FilePath }
        deriving (Show)


-- | Process an LLVM module.
pipeLlvm 
        :: Llvm.Module 
        -> PipeLlvm 
        -> IO [Error]

pipeLlvm mm pp
 = case pp of
        PipeLlvmPrint sink
         ->     pipeSink (renderIndent $ ppr mm) sink

        PipeLlvmCompile builder llPath sPath oPath mExePath
         -> do  -- Write out the LLVM source file.
                let llSrc       = renderIndent $ ppr mm
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
                buildLlc builder llPath sPath

                -- Assemble .s file into .o file
                buildAs builder  sPath  oPath

                -- Link .o file into an executable if we were asked for one.      
                (case mExePath of
                  Nothing -> return ()
                  Just exePath
                   -> do buildLdExe builder oPath exePath
                         return ())

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

