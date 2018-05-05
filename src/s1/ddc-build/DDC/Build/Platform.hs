
module DDC.Build.Platform
        ( Platform      (..)
        , staticFileExtensionOfPlatform
        , sharedFileExtensionOfPlatform

        , Arch          (..)
        , archPointerWidth

        , Os            (..)

        -- * Host platform
        , determineHostPlatform
        , determineHostArch
        , determineHostOs
        , determineHostLlvmVersion
        , determineHostLlvmBinPath )
where
import DDC.Data.Pretty
import Control.Exception         as Exception
import Data.Char                 as Char
import Data.List                 as List
import qualified System.Exit     as System
import qualified System.FilePath as FilePath
import qualified System.Process  as System


-------------------------------------------------------------------------------
-- | Describes a build or target platform.
data Platform
        = Platform
        { platformArch  :: Arch
        , platformOs    :: Os }
        deriving (Eq, Show)

instance Pretty Platform where
 ppr platform
  = vcat
  [ text "Processor Architecture : " <> ppr (platformArch platform)
  , text "Operating System       : " <> ppr (platformOs   platform) ]


-- | Get the file extension to use for a static library on this platform.
staticFileExtensionOfPlatform :: Platform -> String
staticFileExtensionOfPlatform pp
 = case platformOs pp of
        OsDarwin{}      -> "a"
        OsLinux         -> "a"
        OsCygwin        -> "a"
        OsMingw         -> "a"


-- | Get the file extension to use for a shared library on this platform.
sharedFileExtensionOfPlatform :: Platform -> String
sharedFileExtensionOfPlatform pp
 = case platformOs pp of
        OsDarwin{}      -> "dylib"
        OsLinux         -> "so"
        OsCygwin        -> "so"
        OsMingw         -> "dll"


-------------------------------------------------------------------------------
-- | Processor Architecture.
data Arch
        = ArchX86_32
        | ArchX86_64
        | ArchPPC_32
        | ArchPPC_64
        deriving (Eq, Show)

instance Pretty Arch where
 ppr arch
  = case arch of
        ArchX86_32      -> text "x86 32-bit"
        ArchX86_64      -> text "x86 64-bit"
        ArchPPC_32      -> text "PPC 32-bit"
        ArchPPC_64      -> text "PPC 64-bit"


-- | Get the width of a pointer on the architecture, in bits.
archPointerWidth :: Arch -> Int
archPointerWidth arch
 = case arch of
        ArchX86_32      -> 32
        ArchX86_64      -> 64
        ArchPPC_32      -> 32
        ArchPPC_64      -> 64


-------------------------------------------------------------------------------
-- | Operating System.
data Os
        -- | Darwin, including the major, minor and patch numbers,
        --   if specified.
        = OsDarwin (Maybe (Int, Int, Int))

        -- | Generic Linux.
        | OsLinux

        -- | Cygwin on Windows.
        | OsCygwin

        -- | MinGW  on Windows.
        | OsMingw
        deriving (Eq, Show)

instance Pretty Os where
 ppr os
  = case os of
        OsDarwin{}      -> text "Darwin"
        OsLinux         -> text "Linux"
        OsCygwin        -> text "Cygwin"
        OsMingw         -> text "Mingw"


-- Determinators --------------------------------------------------------------
-- | Determine the default host platform.
--
--   Uses the @arch@ and @uname@ commands which must be in the current path.
--
--   Returns `Nothing` if @arch@ or @uname@ cannot be found, returned
--   an error, or we didn't recognise their response.
--
--   For Platforms like Darwin which can run both 32-bit and 64-bit binaries,
--   we return whatever the default is reported by 'arch' and 'uname'.
determineHostPlatform :: IO (Maybe Platform)
determineHostPlatform
 = do   mArch   <- determineHostArch
        mOs     <- determineHostOs

        case (mArch, mOs) of
         (Just arch, Just os)   -> return $ Just (Platform arch os)
         _                      -> return Nothing


-- | Determine the host archicture.
--   Uses the 'arch' command which must be in the current path.
determineHostArch :: IO (Maybe Arch)
determineHostArch
 = do   (exitCode, strArch, _)
         <- System.readProcessWithExitCode "uname" ["-m"] ""

        let result
                | System.ExitFailure{} <- exitCode
                = Nothing

                | isPrefixOf "i386"   strArch   = Just ArchX86_32
                | isPrefixOf "i486"   strArch   = Just ArchX86_32
                | isPrefixOf "i586"   strArch   = Just ArchX86_32
                | isPrefixOf "i686"   strArch   = Just ArchX86_32
                | isPrefixOf "x86_64" strArch   = Just ArchX86_64
                | isPrefixOf "ppc"    strArch   = Just ArchPPC_32
                | isPrefixOf "ppc64"  strArch   = Just ArchPPC_64
                | otherwise                     = Nothing

        return result


-- | Determine the host OS.
--   Uses the 'uname' command which must be in the current path.
determineHostOs :: IO (Maybe Os)
determineHostOs
 = do   (exitCode, strOs, _)
         <- System.readProcessWithExitCode "uname" [] ""

        case exitCode of
         System.ExitFailure{}
          -> return Nothing

         System.ExitSuccess
          |  isPrefixOf "Darwin" strOs  -> determineHostOsDarwin
          |  isPrefixOf "Linux"  strOs  -> return $ Just OsLinux
          |  isPrefixOf "CYGWIN" strOs  -> return $ Just OsCygwin
          |  isPrefixOf "MINGW"  strOs  -> return $ Just OsMingw
          |  otherwise                  -> return Nothing


-- | Given that we're running on Darwin, determine the version numbers.
determineHostOsDarwin :: IO (Maybe Os)
determineHostOsDarwin
 = do   (exitCode, strVersion, _)
         <- System.readProcessWithExitCode "uname" ["-r"] ""

        case exitCode of
         System.ExitFailure{}
          -> return Nothing

         System.ExitSuccess
          | (dsMajor, '.' : rest1) <- List.span isDigit strVersion
          , nMajor                 <- read dsMajor
          , (dsMinor, '.' : rest2) <- List.span isDigit rest1
          , nMinor                 <- read dsMinor
          , (dsPatch, _)           <- List.span isDigit rest2
          , nPatch                 <- read dsPatch
          -> return $ Just $ OsDarwin (Just (nMajor, nMinor, nPatch))

          | otherwise
          -> return $ Nothing


-- | Determine the host LLVM version string, e.g. "3.5.2".
--
--   Takes the path to the 'llvm-config' executable which should be used, or
--   `Nothing` to use whatever is in the current path.
--
determineHostLlvmVersion :: Maybe FilePath -> IO (Maybe String)
determineHostLlvmVersion mpath
 = do
        mbin
         <- determineHostLlvmBinPath mpath

        case mbin of
          Nothing  -> return Nothing
          Just bin -> do

            (exitCode, stdout, _)
             <- System.readProcessWithExitCode (bin FilePath.</> "llvm-config") ["--version"] []

            case exitCode of
              System.ExitFailure{}
                -> return Nothing

              System.ExitSuccess
                -> case lines stdout of
                     [str] -> return (Just str)   -- strip trailing newline
                     _     -> return Nothing


-- | Determine the path to the LLVM executables.
--
--   Takes the path to the 'llvm-config' executable which should be used,
--   otherwise searches in the current PATH. If a suitable version is located,
--   the result of 'llvm-config --bindir' is returned.
--
--   This directory contains the "raw" executables; i.e. without a version
--   suffix.
--
determineHostLlvmBinPath :: Maybe FilePath -> IO (Maybe FilePath)
determineHostLlvmBinPath mpath
  = do
        let
            -- The default list of candidates, tested in order (after any
            -- user-specified path). Add other supported versions here.
            --
            candidates =
              [ "llvm-config-6.0"
              , "llvm-config-5.0"
              , "llvm-config-4.0"
              , "llvm-config"
              ]

            -- Test each candidate and return the result of "llvm-config --bindir"
            search []          = return Nothing
            search (path:rest) = do
              (exitCode, stdout, _)
               <- System.readProcessWithExitCode path ["--bindir"] []
                    -- The user input might be malformed; e.g. '/usr/bin' rather
                    -- than '/usr/bin/llvm-config'
                    `catch`
                    \e -> return (System.ExitFailure (-1), [], show (e :: SomeException))

              case exitCode of
                System.ExitFailure{}
                 -> search rest

                System.ExitSuccess
                 -> case lines stdout of
                      [bindir]  -> return (Just bindir)  -- strip trailing newline
                      _         -> search rest

        -- return the first successful candidate
        search (maybe candidates (: candidates) mpath)

