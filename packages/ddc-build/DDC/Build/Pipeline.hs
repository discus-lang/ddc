{-# LANGUAGE GADTs #-}
-- | A pipeline is an abstraction of a single compiler pass.
--
--  NOTE: The Haddock documentation on pipeline constructors is missing
--        because Haddock does not support commenting GADTs.
--        See the source code for documentation.
--
module DDC.Build.Pipeline
        ( -- * Errors
          Error(..)

          -- * Source code
        , PipeText        (..)
        , pipeText

          -- * Generic Core modules
        , PipeCore        (..)
        , pipeCore

          -- * Core Lite modules
        , PipeLite        (..)
        , pipeLite

          -- * Core Salt modules
        , PipeSalt        (..)
        , pipeSalt

          -- * LLVM modules
        , PipeLlvm        (..)
        , pipeLlvm

          -- * Emitting output
        , Sink                  (..)
        , pipeSink)
where
import DDC.Build.Language
import DDC.Build.Builder
import DDC.Core.Simplifier
import DDC.Base.Pretty
import DDC.Data.Canned
import DDC.Core.Check                           (AnTEC)
import qualified DDC.Core.Transform.Reannotate  as C
import qualified DDC.Core.Fragment              as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Load                  as CL
import qualified DDC.Core.Llvm.Convert          as Llvm
import qualified DDC.Core.Salt.Transfer         as Salt
import qualified DDC.Core.Salt.Platform         as Salt
import qualified DDC.Core.Salt.Runtime          as Salt
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Core.Lite                  as Lite
import qualified DDC.Llvm.Module                as Llvm
import qualified Control.Monad.State.Strict     as S
import Control.Monad
import Control.DeepSeq
import System.Directory

-- Error ----------------------------------------------------------------------
data Error
        = ErrorSaltLoad    (CL.Error Salt.Name)

        -- | Error converting the module to Disciple Core Salt.
        | forall err. Pretty err => ErrorSaltConvert !err

        -- | Error converting the module to Disciple Core Lite.
        | forall err. Pretty err => ErrorLiteConvert !err

        -- | Error when loading a module.
        --   Blame it on the user.
        | forall err. Pretty err => ErrorLoad !err

        -- | Error when type checking a transformed module.
        --   Blame it on the compiler.
        | forall err. Pretty err => ErrorLint !err


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

instance NFData Error


-- PipeSource -----------------------------------------------------------------
-- | Process program text.
data PipeText n (err :: * -> *) where
  PipeTextOutput 
        :: !Sink
        -> PipeText n err

  PipeTextLoadCore 
        :: (Ord n, Show n, Pretty n)
        => !(Fragment n err)
        -> ![PipeCore (C.AnTEC () n) n]
        -> PipeText n err


-- | Process a text module.
--
--   Returns empty list on success.
pipeText
        :: NFData n
        => String
        -> Int
        -> String
        -> PipeText n err
        -> IO [Error]

pipeText !srcName !srcLine !str !pp
 = case pp of
        PipeTextOutput !sink
         -> {-# SCC "PipeTextOutput" #-}
            pipeSink str sink

        PipeTextLoadCore !frag !pipes
         -> {-# SCC "PipeTextLoadCore" #-}
            let toks            = fragmentLexModule frag srcName srcLine str
            in case CL.loadModule (fragmentProfile frag) srcName toks of
                 Left err -> return $ [ErrorLoad err]
                 Right mm -> pipeCores mm pipes


-- PipeCoreModule -------------------------------------------------------------
-- | Process a core module.
data PipeCore a n where
  -- Plumb the module on without transforming it.
  PipeCoreId
        :: ![PipeCore a n]
        -> PipeCore a n

  -- Output a module to console or file.
  PipeCoreOutput    
        :: !Sink 
        -> PipeCore a n

  -- Type check a module.
  PipeCoreCheck      
        :: !(Fragment n err)
        -> ![PipeCore (C.AnTEC a n) n]
        -> PipeCore a n

  -- Type check a module, discarding previous per-node type annotations.
  PipeCoreReCheck
        :: (Show a, NFData a)
        => !(Fragment n err)
        -> ![PipeCore (C.AnTEC a n)  n]
        -> PipeCore  (C.AnTEC a n') n

  -- Strip annotations from a module.
  PipeCoreStrip
        :: ![PipeCore () n]
        ->  PipeCore a n

  -- Apply a simplifier to a module.
  PipeCoreSimplify  
        :: !(Fragment n err)
        -> !s
        -> !(Simplifier s a n)
        -> ![PipeCore () n] 
        -> PipeCore a n

  -- Treat a module as belonging to the Core Lite fragment from now on.
  PipeCoreAsLite
        :: ![PipeLite]
        -> PipeCore (C.AnTEC () Lite.Name) Lite.Name

  -- Treat a module as beloning to the Core Salt fragment from now on.
  PipeCoreAsSalt
        :: Pretty a 
        => ![PipeSalt a] 
        -> PipeCore a Salt.Name

  -- Apply a canned function to a module.
  -- This is helpful for debugging, and tweaking the output before pretty printing.
  -- More reusable transforms should be made into their own pipeline stage.
  PipeCoreHacks
        :: Canned (C.Module a n -> IO (C.Module a n))
        -> ![PipeCore a n]
        -> PipeCore a n


-- | Process a Core module.
--
--   Returns empty list on success.
pipeCore
        :: (NFData a, Show a, NFData n, Eq n, Ord n, Show n, Pretty n)
        => C.Module a n
        -> PipeCore a n
        -> IO [Error]

pipeCore !mm !pp
 = case pp of
        PipeCoreId !pipes
         -> {-# SCC "PipeCoreId" #-}
            pipeCores mm pipes

        PipeCoreOutput !sink
         -> {-# SCC "PipeCoreOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeCoreCheck !fragment !pipes
         -> {-# SCC "PipeCoreCheck" #-}
            let profile         = fragmentProfile fragment

                goCheck mm1
                 = case C.checkModule (C.configOfProfile profile) mm1 of
                        Left err   -> return [ErrorLint err]
                        Right mm2  -> goComplies mm2

                goComplies mm1
                 = case C.complies profile mm1 of
                        Just err   -> return [ErrorLint err]
                        Nothing    -> pipeCores mm1 pipes

             in goCheck mm

        PipeCoreReCheck !fragment !pipes
         -> {-# SCC "PipeCoreReCheck" #-}
            pipeCore (C.reannotate C.annotTail mm)
         $  PipeCoreCheck fragment pipes

        PipeCoreStrip !pipes
         -> {-# SCC "PipeCoreStrip" #-}
            let mm' = (C.reannotate (const ()) mm)
            in  pipeCores mm' pipes

        PipeCoreSimplify !fragment !nameZero !simpl !pipes
         -> {-# SCC "PipeCoreSimplify" #-}
            let profile         = fragmentProfile fragment
                primKindEnv     = C.profilePrimKinds      profile
                primTypeEnv     = C.profilePrimTypes      profile

                !mm'		= (flip S.evalState nameZero
				   $ applySimplifier profile primKindEnv primTypeEnv simpl mm)

                !mm2            = C.reannotate (const ()) mm'

                -- NOTE: It is helpful to deepseq here so that we release 
                --       references to the unsimplified version of the code.
                --       Because we've just applied reannotate, we also
                --       release type annotations on the expression tree.
            in  mm2 `deepseq` pipeCores mm2 pipes

        PipeCoreAsLite !pipes
         -> {-# SCC "PipeCoreAsLite" #-}
            liftM concat $ mapM (pipeLite mm) pipes

        PipeCoreAsSalt !pipes
         -> {-# SCC "PipeCoreAsSalt" #-}
            liftM concat $ mapM (pipeSalt mm) pipes

        PipeCoreHacks !(Canned f) !pipes
         -> {-# SCC "PipeCoreHacks" #-} 
            do  mm'     <- f mm
                pipeCores mm' pipes


pipeCores :: (NFData a, Show a, NFData n, Eq n, Ord n, Show n, Pretty n)
          => C.Module a n -> [PipeCore a n] -> IO [Error]

pipeCores !mm !pipes 
 = go [] pipes
 where  go !errs []   
         = return errs

        go !errs (pipe : rest)
         = do   !err     <- pipeCore mm pipe
                go (errs ++ err) rest


-- PipeLiteModule -------------------------------------------------------------
-- | Process a Core Lite module.
data PipeLite
        -- | Output the module in core language syntax.
        = PipeLiteOutput !Sink

        -- | Convert the module to the Core Salt Fragment.
        | PipeLiteToSalt !Salt.Platform 
                         !Salt.Config
                         ![PipeCore () Salt.Name]


-- | Process a Core Lite module.
pipeLite :: C.Module (C.AnTEC () Lite.Name) Lite.Name
         -> PipeLite
         -> IO [Error]

pipeLite !mm !pp
 = case pp of
        PipeLiteOutput !sink
         -> {-# SCC "PipeLiteOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeLiteToSalt !platform !runConfig !pipes
         -> {-# SCC "PipeLiteToSalt" #-}
            case Lite.saltOfLiteModule platform runConfig 
                        (C.profilePrimDataDefs Lite.profile) 
                        (C.profilePrimKinds    Lite.profile)
                        (C.profilePrimTypes    Lite.profile)
                        mm 
             of  Left  err  -> return [ErrorLiteConvert err]
                 Right mm'  -> pipeCores mm' pipes 

-- PipeSaltModule --------------------------------------------------------------
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


-- PipeLlvmModule -------------------------------------------------------------
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


-- | Emit a string to the given `Sink`.
pipeSink :: String -> Sink -> IO [Error]
pipeSink !str !tg
 = case tg of
        SinkDiscard
         -> do  return []

        SinkStdout
         -> do  putStrLn str
                return []

        SinkFile path
         -> do  writeFile path str
                return []

