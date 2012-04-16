
module DDCI.Core.Build.Builder
        ( BuilderConfig (..)
        , defaultBuilderConfig

        , Builder       (..)
        , builders

        , determineDefaultBuilder)
where
import DDCI.Core.Build.Platform
import System.Cmd
import System.FilePath

-- | Configuration information for a builder that is not platform specific.
data BuilderConfig
        = BuilderConfig
        { -- | Directory that holds the runtime system libraries.
          builderConfigRuntime  :: FilePath }


-- | Actions to use to invoke external compilation tools.
data Builder
        = Builder
        { -- | The name of this platform.
          builderName   :: String

          -- | The platform the build is being performed on.
        , buildHost     :: Platform

          -- | The platform we're compiling code for.
        , buildTarget   :: Platform

          -- | Invoke the LLVM compiler
          --   to compile a .ll file into a .s file.
        , buildLlc      :: FilePath -> FilePath -> IO Bool 

          -- | Invoke the system assembler
          --   to assemble a .s file into a .o file.
        , buildAs       :: FilePath -> FilePath -> IO Bool 

          -- | Link an executable.
        , buildLdExe    :: FilePath -> FilePath -> IO Bool }

instance Show Builder where
 show builder
        = "Builder " ++ show (builderName builder)


-- builders -------------------------------------------------------------------
-- | All supported builders.
--   The host and target platforms are the same.
builders :: BuilderConfig -> [Builder]
builders config
 =      [ builder_X8632_Darwin config
        , builder_X8664_Darwin config
        , builder_X8632_Linux  config 
        , builder_X8664_Linux  config ]


defaultBuilderConfig
        = BuilderConfig
        { builderConfigRuntime  = "packages/ddc-core-sea/runtime" }


-- defaultBuilder -------------------------------------------------------------
-- | Determine the default builder based on the 'arch' and 'uname' commands.
--   This assumes that the 'host' and 'target' platforms are the same.
--
--   If we don't recognise the result of 'arch' or 'uname', or don't have 
--   a default builder config for this platform then `Nothing`.
determineDefaultBuilder :: BuilderConfig -> IO (Maybe Builder)
determineDefaultBuilder config
 = do   mPlatform       <- determineHostPlatform

        case mPlatform of
         Just (Platform ArchX86_32 OsDarwin)    
                -> return $ Just (builder_X8632_Darwin config)

         Just (Platform ArchX86_64 OsDarwin)    
                -> return $ Just (builder_X8664_Darwin config)

         _      -> return Nothing


-- x86_32-darwin ----------------------------------------------------------------
builder_X8632_Darwin config
 =      Builder 
        { builderName   = "x86_32-darwin" 
        , buildHost     = Platform ArchX86_32 OsDarwin
        , buildTarget   = Platform ArchX86_32 OsDarwin

        , buildLlc    
                = \llFile sFile
                -> doCmd $ "llc -O3 -march=x86 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd $  "as -arch i386"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe
                = \oFile binFile
                -> doCmd $  "gcc -m32" 
                ++ " -o " ++ binFile
                ++ " "    ++ oFile
                ++ " "    ++ (builderConfigRuntime config </> "libddc-runtime.dylib")
        }


-- x86_64-darwin --------------------------------------------------------------
builder_X8664_Darwin config
 =      Builder
        { builderName   = "x86_64-darwin"
        , buildHost     = Platform ArchX86_64 OsDarwin
        , buildTarget   = Platform ArchX86_64 OsDarwin

        , buildLlc    
                = \llFile sFile
                -> doCmd $ "llc -O3 -march=x86-64 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd $  "as -arch x86_64"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd $  "gcc -m64" 
                ++ " -o " ++ binFile
                ++ " "    ++ oFile
                ++ " "    ++ (builderConfigRuntime config </> "libddc-runtime.dylib")
        }


-- x86_32-linux ---------------------------------------------------------------
builder_X8632_Linux config
 =      Builder
        { builderName   = "x86_32-linux"
        , buildHost     = Platform ArchX86_32 OsLinux
        , buildTarget   = Platform ArchX86_32 OsLinux

        , buildLlc    
                = \llFile sFile
                -> doCmd $ "llc -O3 -march=x86 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd $  "as -arch i386"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd $  "gcc -m32" 
                ++ " -o " ++ binFile
                ++ " "    ++ oFile
                ++ " "    ++ (builderConfigRuntime config </> "libddc-runtime.so")
        }


-- x86_64-linux ---------------------------------------------------------------
builder_X8664_Linux config
 =      Builder
        { builderName   = "x86_64-linux"
        , buildHost     = Platform ArchX86_32 OsLinux
        , buildTarget   = Platform ArchX86_32 OsLinux

        , buildLlc    
                = \llFile sFile
                -> doCmd $ "llc -O3 -march=x86_64 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd $  "as -arch x86_64"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd $  "gcc -m64" 
                ++ " -o " ++ binFile
                ++ " "    ++ oFile
                ++ " "    ++ (builderConfigRuntime config </> "libddc-runtime.so")
        }


-- Utils ----------------------------------------------------------------------
doCmd :: String -> IO Bool
doCmd cmd
 = do   _ <- system cmd
        return True
