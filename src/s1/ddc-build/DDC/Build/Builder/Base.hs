{-# OPTIONS_HADDOCK hide #-}

module DDC.Build.Builder.Base
        ( BuilderConfig (..)
        , BuilderHost   (..)
        , Builder       (..)
        , BuilderResult (..)

        -- Exported tools for for builder definition modules.
        , doCmd
        , module DDC.Build.Platform
        , module System.FilePath
        , module Data.List)
where
import DDC.Build.Platform
        (Platform(..), Arch(..), Os(..))

import DDC.Data.Pretty
import System.Exit                              hiding (die)
import System.Process
import System.FilePath
import Data.List
import qualified DDC.Core.Salt.Platform         as Llvm


-------------------------------------------------------------------------------
-- | Configuration information for a builder that is not platform specific.
data BuilderConfig
        = BuilderConfig
        { -- | Directory that holds the source for the runtime system
          --   and base library.
          builderConfigBaseSrcDir       :: FilePath

          -- | Directory that holds the shared objects for the runtime
          --   system and base library.
        , builderConfigBaseLibDir       :: FilePath

          -- | Link the runtime library statically,
          --   otherwise link dynamically.
        , builderConfigLinkStatic       :: Bool }


-- | Builder information that we determine by interrogating the host platform.
--   This tells us what we need to know about the environment that we're
--   building in, versions of software tools etc. This is separate
data BuilderHost
        = BuilderHost
        { -- | Version of the LLVM compiler suite we are using.
          builderHostLlvmVersion        :: String

          -- | Path to the LLVM executables.
        , builderHostLlvmBinPath        :: FilePath }
        deriving Show


-- | Actions to use to invoke external compilation tools.
data Builder
        = Builder
        { -- | The name of this platform.
          builderName           :: String

          -- | The platform the build is being performed on.
        , buildHost             :: Platform

          -- | The platform we're compiling code for.
        , buildTarget           :: Platform

          -- | The LLVM target specification.
          --   Gives the widths of pointers and primitive numeric types.
        , buildSpec             :: Llvm.Platform

          -- | Directory that holds the source for the runtime system
          --   and base library.
        , buildBaseSrcDir       :: FilePath

          -- | Directory that holds the shared objects for the runtime
          --   system and base library.
        , buildBaseLibDir       :: FilePath

          -- | Invoke the C compiler
          --   to compile a .c file into a .o file.
        , buildCC               :: FilePath -> FilePath -> IO ()

          -- | Invoke the LLVM compiler
          --   to compile a .ll file into a .s file.
        , buildLlc              :: FilePath -> FilePath -> IO ()

          -- | Version string of the LLVM compiler suite we are using.
        , buildLlvmVersion      :: String

          -- | Invoke the system assembler
          --   to assemble a .s file into a .o file.
        , buildAs               :: FilePath -> FilePath -> IO ()

          -- | Link an executable.
        , buildLdExe            :: [FilePath] -> FilePath -> IO ()

          -- | Link a static library.
        , buildLdLibStatic      :: [FilePath] -> FilePath -> IO ()

          -- | Link a shared library.
        , buildLdLibShared      :: [FilePath] -> FilePath -> IO () }


-- | The result of a build command.
--
--   We use these so that the called doesn't need to worry about
--   interpreting numeric exit codes.
data BuilderResult
        -- | Build command completed successfully.
        = BuilderSuccess

        -- | Build command was cancelled or killed by the user.
        --   eg by Control-C on the console.
        | BuilderCanceled

        -- | Build command failed.
        --   There is probably something wrong with the generated file.
        --   Unrecognised exit codes also result in this BuilderResult.
        | BuilderFailed
        deriving (Show, Eq)


-------------------------------------------------------------------------------
instance Show Builder where
 show builder
        = "Builder " ++ show (builderName builder)


instance Pretty Builder where
 ppr builder
        = vcat
        [ text "Builder Name : " <> string (builderName builder)
        , mempty
        , text "Host Platform"
        , indent 1 $ ppr $ buildHost builder
        , mempty
        , text "Target Platform"
        , indent 1 $ ppr $ buildTarget builder
        , mempty
        , text "LLVM Target Spec"
        , indent 1 $ ppr $ buildSpec builder ]


-- Utils ----------------------------------------------------------------------
-- | Run a system command, and if it fails quit the program.
doCmd   :: String                       -- ^ Description of tool being invoked.
        -> [(Int, BuilderResult)]       -- ^ How to interpret exit codes.
        -> [String]                     -- ^ System command to run.
        -> IO ()

doCmd thing exitCodeMeanings cmdParts
 = do   code <- system cmd
        case code of
         ExitSuccess
          -> return ()

         ExitFailure c
          |  Just meaning        <- lookup c exitCodeMeanings
          -> case meaning of
                BuilderSuccess  -> return ()
                BuilderCanceled -> exitWith $ ExitFailure 2
                BuilderFailed   -> die c
          | otherwise           -> die c

 where  cmd   = unwords cmdParts
        die c = error $ unlines
              [ "System command failed when invoking external " ++ thing ++ "."
              , " Command was: " ++ cmd
              , " Exit code:   " ++ show c ]

