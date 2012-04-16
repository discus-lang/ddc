
module DDCI.Core.Build.Platform
        ( Platform      (..)
        , Arch          (..)
        , Os            (..)

        -- * Host platform
        , determineHostPlatform
        , determineHostArch
        , determineHostOs)
where
import System.Process
import System.Exit
import Data.List

-- | Describes a build or target platform.
data Platform
        = Platform
        { platformArch  :: Arch
        , platformOs    :: Os }
        deriving (Eq, Show)

-- | Processor Architecture.
data Arch
        = ArchX86_32
        | ArchX86_64
        | ArchPPC_32
        | ArchPPC_64
        deriving (Eq, Show)


-- | Operating System.
data Os
        = OsDarwin
        | OsLinux
        | OsCygwin
        deriving (Eq, Show)



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

        putStrLn $ show (mArch, mOs)

        case (mArch, mOs) of
         (Just arch, Just os)   -> return $ Just (Platform arch os)
         _                      -> return Nothing


-- | Determine the host archicture.
--   Uses the 'arch' command which must be in the current path.
determineHostArch :: IO (Maybe Arch)
determineHostArch
 = do   (exitCode, strArch, _) 
         <- readProcessWithExitCode "uname" ["-m"] ""

        let result
                | ExitFailure{} <- exitCode     = Nothing
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
         <- readProcessWithExitCode "uname" [] ""
        
        let result
                | ExitFailure{} <- exitCode     = Nothing
                | isPrefixOf "Darwin" strOs     = Just OsDarwin
                | isPrefixOf "Linux"  strOs     = Just OsLinux
                | isPrefixOf "CYGWIN" strOs     = Just OsCygwin
                | otherwise                     = Nothing

        return result

