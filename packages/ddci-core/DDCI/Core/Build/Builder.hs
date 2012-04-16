
module DDCI.Core.Build.Builder
        ( Builder (..)
        , builders
        , determineDefaultBuilder)
where
import DDCI.Core.Build.Platform
import System.Cmd

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
        , buildLdExe:: FilePath -> FilePath -> IO Bool }

instance Show Builder where
 show builder
        = "Builder " ++ show (builderName builder)


-- | All supported builders.
--   The host and target platforms are the same.
builders :: [Builder]
builders
 =      [ builder_X8632_Darwin
        , builder_X8664_Darwin ]


-- | Determine the default builder based on the 'arch' and 'uname' commands.
--   This assumes that the 'host' and 'target' platforms are the same.
--
--   If we don't recognise the result of 'arch' or 'uname', or don't have 
--   a default builder config for this platform then `Nothing`.
determineDefaultBuilder :: IO (Maybe Builder)
determineDefaultBuilder
 = do   mPlatform       <- determineHostPlatform

        case mPlatform of
         Just (Platform ArchX86    OsDarwin)    -> return $ Just builder_X8632_Darwin
         Just (Platform ArchX86_64 OsDarwin)    -> return $ Just builder_X8664_Darwin
         _                                      -> return Nothing


-- x86_32-darwin ----------------------------------------------------------------
builder_X8632_Darwin
 =      Builder 
        { builderName   = "x86_32-darwin" 
        , buildHost   = Platform ArchX86 OsDarwin
        , buildTarget = Platform ArchX86 OsDarwin

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
                ++ " "    ++ "packages/ddc-core-sea/runtime/libddc-runtime.dylib"
        }


-- x86_64-darwin --------------------------------------------------------------
builder_X8664_Darwin
 =      Builder
        { builderName   = "x86_64-darwin"
        , buildHost   = Platform ArchX86_64 OsDarwin
        , buildTarget = Platform ArchX86_64 OsDarwin

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
                ++ " "    ++ "packages/ddc-core-sea/runtime/libddc-runtime.dylib"
        }


-- Utils ----------------------------------------------------------------------
doCmd :: String -> IO Bool
doCmd cmd
 = do   _ <- system cmd
        return True
