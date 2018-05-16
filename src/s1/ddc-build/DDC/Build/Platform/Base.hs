
module DDC.Build.Platform.Base
        ( Platform (..)
        , staticFileExtensionOfPlatform
        , sharedFileExtensionOfPlatform

        , Arch (..)
        , archPointerWidth

        , Os (..))
where
import DDC.Data.Pretty


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
  [ text "Processor Architecture: " <> ppr (platformArch platform)
  , text "Operating System:       " <> ppr (platformOs   platform) ]


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

