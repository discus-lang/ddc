
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
        , determineHostOs)
where
import DDC.Base.Pretty
import System.Process
import System.Exit
import Data.List


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
        OsDarwin        -> "a"
        OsLinux         -> "a"
        OsCygwin        -> "a"
        OsMingw         -> "a"


-- | Get the file extension to use for a shared library on this platform.
sharedFileExtensionOfPlatform :: Platform -> String
sharedFileExtensionOfPlatform pp
 = case platformOs pp of
        OsDarwin        -> "dylib"
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
        = OsDarwin
        | OsLinux
        | OsCygwin
        | OsMingw
        deriving (Eq, Show)

instance Pretty Os where
 ppr os
  = case os of
        OsDarwin        -> text "Darwin"
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
                | isPrefixOf "MINGW"  strOs     = Just OsMingw
                | otherwise                     = Nothing

        return result

