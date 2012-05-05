{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline
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

          -- * Processing Core Salt modules
        , PipeSalt        (..)
        , pipeSalt

          -- * Processing LLVM modules
        , PipeLlvm        (..)
        , pipeLlvm

          -- * Emitting output
        , Sink                  (..)
        , pipeSink)
where
import DDC.Build.Language
import DDC.Build.Builder
import DDC.Core.Fragment.Profile
import DDC.Core.Simplifier
import DDC.Core.Collect
import DDC.Base.Pretty
import DDC.Data.Canned
import qualified DDC.Core.Fragment.Compliance   as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Load                  as CL
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Salt.Platform         as Salt
import qualified DDC.Core.Salt.Lite             as Lite
import qualified DDC.Core.Salt.Output           as Output
import qualified DDC.Llvm.Module                as Llvm
import qualified DDC.Type.Env                   as Env
import qualified Control.Monad.State.Strict     as S
import Control.Monad

-- Error ----------------------------------------------------------------------
data Error
        = ErrorSaltLoad    (CL.Error Output.Name)
        | ErrorSaltConvert (Output.Error ())

        | ErrorLiteConvert  (Lite.Error ())

        -- | Error when loading a module.
        --   Blame it on the user.
        | forall err. Pretty err => ErrorLoad  err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint err


instance Pretty Error where
 ppr err
  = case err of
        ErrorSaltLoad err'
         -> vcat [ text "Type error when loading Salt module."
                 , indent 2 (ppr err') ]

        ErrorSaltConvert err'
         -> vcat [ text "Fragment violation when converting Salt module to C code."
                 , indent 2 (ppr err') ]

        ErrorLiteConvert err'
         -> vcat [ text "Fragment violation when converting Lite module to Salt module."
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
        :: String
        -> Int
        -> String
        -> PipeText n err
        -> IO [Error]

pipeText srcName srcLine str pp
 = case pp of
        PipeTextOutput sink
         -> pipeSink str sink

        PipeTextLoadCore frag pipes
         -> let toks            = fragmentLexModule frag srcName srcLine str
            in case CL.loadModule (fragmentProfile frag) srcName toks of
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
  PipeCoreAsSalt
        :: [PipeSalt] 
        -> PipeCore Output.Name

  -- Apply a canned function to a module.
  -- This is helpful for debugging, and tweaking the output before pretty printing.
  -- More reusable transforms should be made into their own pipeline stage.
  PipeCoreHacks
        :: (Canned (C.Module () n -> IO (C.Module () n)))
        -> [PipeCore n]
        -> PipeCore n

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

                goCheck mm1
                 = case C.checkModule primDataDefs primKindEnv primTypeEnv mm1 of
                        Left err   -> return [ErrorLint err]
                        Right mm2  -> goComplies mm2

                goComplies mm1
                 = case C.complies profile mm1 of
                        Just err   -> return [ErrorLint err]
                        Nothing    -> liftM concat $ mapM (pipeCore mm1) pipes

             in goCheck mm

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

        PipeCoreAsSalt pipes
         -> liftM concat $ mapM (pipeSalt mm) pipes

        PipeCoreHacks (Canned f) pipes
         -> do  mm'     <- f mm
                liftM concat $ mapM (pipeCore mm') pipes


-- PipeLiteModule -------------------------------------------------------------
-- | Process a Core Lite module.
data PipeLite
        -- | Output the module in core language syntax.
        = PipeLiteOutput    Sink

        -- | Convert the module to the Core Salt Fragment.
        | PipeLiteToSalt     Salt.Platform [PipeCore Output.Name]
        deriving Show

pipeLite :: C.Module () Lite.Name
         -> PipeLite
         -> IO [Error]

pipeLite mm pp
 = case pp of
        PipeLiteOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeLiteToSalt platform pipes
         -> case Lite.toSalt platform (profilePrimDataDefs Lite.profile) mm of
                Left  err       -> return [ErrorLiteConvert err]
                Right mm'       -> liftM concat $ mapM (pipeCore mm') pipes


-- PipeSaltModule --------------------------------------------------------------
-- | Process a Core Salt module.
data PipeSalt
        -- | Output the module in core language syntax.
        = PipeSaltOutput     Sink

        -- | Print the module as a C source code.
        | PipeSaltPrint      
        { pipeWithSaltPrelude  :: Bool
        , pipeModuleSink       :: Sink }

        -- | Convert the module to LLVM.
        | PipeSaltToLlvm        Salt.Platform [PipeLlvm]
        deriving (Show)


-- | Process a Core Salt module.
pipeSalt  :: C.Module () Output.Name 
          -> PipeSalt
          -> IO [Error]

pipeSalt mm pp
 = case pp of
        PipeSaltOutput sink
         -> pipeSink (renderIndent $ ppr mm) sink

        PipeSaltPrint withPrelude sink
         -> case Output.convertModule mm of
                Left  err 
                 ->     return $ [ErrorSaltConvert err]

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

        PipeSaltToLlvm platform more
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

