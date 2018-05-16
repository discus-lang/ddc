
module DDC.Build.Platform.Determine
        ( determineHostPlatform
        , determineHostArch
        , determineHostOs
        , determineHostLlvmVersion
        , determineHostLlvmBinPath )
where
import DDC.Build.Platform.Base
import DDC.Build.Platform.Error
import Control.Exception                as Exception
import Data.Char                        as Char
import Data.List                        as List
import qualified System.FilePath        as FilePath
import qualified System.Exit            as System
import qualified System.Process         as System
import qualified System.Directory       as System


-- Process --------------------------------------------------------------------
-- | Execute a process to check the environment.
execProcess
        :: String       -- ^ Name of the executable.
        -> [String]     -- ^ Arguments to the executable.
        -> IO (Either Error String)

execProcess cmd args
 = goFindTool
 where
        goFindTool
         = do   mPathExe        <- System.findExecutable cmd
                case mPathExe of
                 Nothing        -> return $ Left $ ErrorToolMissing cmd
                 Just path      -> goExecTool path

        goExecTool path
         = do   (exitCode, strOut, strErr)
                  <- System.readProcessWithExitCode path args ""

                case exitCode of
                 System.ExitFailure{}
                   -> return $ Left  $ ErrorToolFailed path strOut strErr
                 _ -> return $ Right $ strOut


-- Determinators --------------------------------------------------------------
-- | Determine the host platform.
--
--   Uses the @uname@ command which must be in the current path.
--
--   For Platforms like Darwin which can run both 32-bit and 64-bit binaries,
--   we return whatever the default is reported by 'uname'.
--
determineHostPlatform :: IO (Either Error Platform)
determineHostPlatform
 = goDetermineArch
 where
        goDetermineArch
         = determineHostArch
         >>= \case
                Left err        -> return $ Left err
                Right arch      -> goDetermineOs arch

        goDetermineOs arch
         = determineHostOs
         >>= \case
                Left err        -> return $ Left err
                Right os        -> return $ Right $ Platform arch os


-- | Determine the host archicture.
--   Uses the 'arch' command which must be in the current path.
determineHostArch :: IO (Either Error Arch)
determineHostArch
 = execProcess "uname" ["-m"]
 >>= \case
        Left  err
         -> return $ Left err

        Right strArch
         |  isPrefixOf "i386"   strArch  -> return $ Right ArchX86_32
         |  isPrefixOf "i486"   strArch  -> return $ Right ArchX86_32
         |  isPrefixOf "i586"   strArch  -> return $ Right ArchX86_32
         |  isPrefixOf "i686"   strArch  -> return $ Right ArchX86_32
         |  isPrefixOf "x86_64" strArch  -> return $ Right ArchX86_64
         |  isPrefixOf "ppc"    strArch  -> return $ Right ArchPPC_32
         |  isPrefixOf "ppc64"  strArch  -> return $ Right ArchPPC_64
         |  otherwise
         -> return $ Left
         $  ErrorUnameUnknownArch "uname -m" strArch


-- | Determine the host OS.
--   Uses the 'uname' command which must be in the current path.
determineHostOs :: IO (Either Error Os)
determineHostOs
 = execProcess "uname" []
 >>= \case
        Left err
         -> return $ Left err

        Right strOs
         |  isPrefixOf "Darwin" strOs    -> determineHostOsDarwin
         |  isPrefixOf "Linux"  strOs    -> return $ Right OsLinux
         |  isPrefixOf "CYGWIN" strOs    -> return $ Right OsCygwin
         |  isPrefixOf "MINGW"  strOs    -> return $ Right OsMingw
         |  otherwise
         -> return $ Left
         $  ErrorUnameUnknownOs "uname" strOs


-- | Given that we're running on Darwin, determine the version numbers.
determineHostOsDarwin :: IO (Either Error Os)
determineHostOsDarwin
 = execProcess "uname" ["-r"]
 >>= \case
        Left err        -> return $ Left err
        Right strVersion
         |  (dsMajor, '.' : rest1) <- List.span isDigit strVersion
         ,  nMajor                 <- read dsMajor
         ,  (dsMinor, '.' : rest2) <- List.span isDigit rest1
         ,  nMinor                 <- read dsMinor
         ,  (dsPatch, _)           <- List.span isDigit rest2
         ,  nPatch                 <- read dsPatch
         -> return $ Right $ OsDarwin (Just (nMajor, nMinor, nPatch))

         |  otherwise
         -> return $ Left
         $  ErrorUnameUnknownDarwinVersion "uname -r" strVersion


-- | Determine the host LLVM version string, e.g. "3.5.2".
--
--   Takes the path to the 'llvm-config' executable which should be used,
--   or `Nothing` to use whatever is in the current path.
--
determineHostLlvmVersion :: Maybe FilePath -> IO (Either Error String)
determineHostLlvmVersion mpath
 = goFindLlvm
 where
        goFindLlvm
         = determineHostLlvmBinPath mpath
         >>= \case
                Left  err       -> return $ Left err
                Right pathBin   -> goCheckLlvm pathBin

        goCheckLlvm pathBin
         = execProcess (pathBin FilePath.</> "llvm-config") ["--version"]
         >>= \case
                Left err        -> return $ Left err
                Right result
                 -> case lines result of
                        -- strip off trailing newline
                        [str]   -> return $ Right str
                        _       -> return $ Left
                                $  ErrorLlvmConfigUnexpectedOutput
                                        "llvm-config --version" result


-- | Determine the path to the LLVM executables.
--
--   Takes the path to the 'llvm-config' executable which should be used,
--   otherwise searches in the current PATH. If a suitable version is
--   located, the result of 'llvm-config --bindir' is returned.
--
--   This directory contains the "raw" executables; i.e. without a version
--   suffix.
--
determineHostLlvmBinPath :: Maybe FilePath -> IO (Either Error FilePath)
determineHostLlvmBinPath mpath
 -- return the first successful candidate
 = search (maybe candidates (: candidates) mpath)
 where
        -- The default list of candidates, tested in order (after any
        -- user-specified path). Add other supported versions here.
        candidates
         =      [ "llvm-config-6.0"
                , "llvm-config-5.0"
                , "llvm-config-4.0"
                , "llvm-config"
                ]

        -- Test each candidate and return
        -- the result of "llvm-config --bindir"
        search []
         = return $ Left
         $ ErrorLlvmConfigMissing mpath candidates

        search (path : rest)
         = do   (exitCode, stdout, _)
                 <- System.readProcessWithExitCode path ["--bindir"] []
                      -- The user input might be malformed; e.g. '/usr/bin'
                      -- rather than '/usr/bin/llvm-config'
                      `catch`
                      \e -> return ( System.ExitFailure (-1)
                                   , [], show (e :: SomeException))

                case exitCode of
                 System.ExitFailure{}
                  -> search rest

                 System.ExitSuccess
                  -> case lines stdout of
                        -- strip trailing newline
                        [bindir]  -> return (Right bindir)
                        _         -> search rest

