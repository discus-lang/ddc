
module DDC.Build.Builder
        ( BuilderConfig (..)
        , defaultBuilderConfig

        , Builder       (..)
        , builders

        , determineDefaultBuilder)
where
import DDC.Build.Platform
import System.FilePath
import System.Exit
import System.Cmd
import qualified DDC.Core.Salt.Platform         as Llvm

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

          -- | The LLVM target specification
          --   Gives the widths of pointers and primitive numeric types.
        , buildSpec     :: Llvm.Platform

          -- | Invoke the LLVM compiler
          --   to compile a .ll file into a .s file.
        , buildLlc      :: FilePath -> FilePath -> IO ()

          -- | Invoke the system assembler
          --   to assemble a .s file into a .o file.
        , buildAs       :: FilePath -> FilePath -> IO ()

          -- | Link an executable.
        , buildLdExe    :: FilePath -> FilePath -> IO () }

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
        { builderConfigRuntime  = "packages/ddc-core-salt/runtime" }


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

	 Just (Platform ArchX86_32 OsLinux)
		-> return $ Just (builder_X8632_Linux  config)

	 Just (Platform ArchX86_64 OsLinux)
		-> return $ Just (builder_X8664_Linux  config)

	 Just (Platform ArchX86_32 OsCygwin)
		-> return $ Just (builder_X8632_Cygwin config)

         _      -> return Nothing


-- x86_32-darwin ----------------------------------------------------------------
builder_X8632_Darwin config
 =      Builder 
        { builderName   = "x86_32-darwin" 
        , buildHost     = Platform ArchX86_32 OsDarwin
        , buildTarget   = Platform ArchX86_32 OsDarwin
        , buildSpec     = Llvm.platform32

        , buildLlc    
                = \llFile sFile
                -> doCmd "LLVM compiler"
                $ "llc -O3 -march=x86 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"
                $  "as -arch i386"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe
                = \oFile binFile
                -> doCmd "linker"
                $  "gcc -m32" 
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
        , buildSpec     = Llvm.platform64

        , buildLlc    
                = \llFile sFile
                -> doCmd "LLVM compiler"
                $ "llc -O3 -march=x86-64 " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"
                $  "as -arch x86_64"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd "linker"
                $  "gcc -m64" 
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
        , buildSpec     = Llvm.platform32

        , buildLlc    
                = \llFile sFile
                -> doCmd "LLVM compiler"
                $ "llc -O3 -march=x86 -relocation-model=pic " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"
                $  "as --32"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd "linker"
                $  "gcc -m32" 
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
        , buildSpec     = Llvm.platform64

        , buildLlc    
                = \llFile sFile
                -> doCmd "LLVM compiler" 
                $ "llc -O3 -march=x86-64 -relocation-model=pic " 
                ++ llFile 
                ++ " -o " ++ sFile

        , buildAs
                = \sFile oFile
                -> doCmd "assembler" 
                $  "as --64"  
                ++ " -o " ++ oFile
                ++ " "    ++ sFile  

        , buildLdExe  
                = \oFile binFile
                -> doCmd "linker" 
                $  "gcc -m64" 
                ++ " -o " ++ binFile
                ++ " "    ++ oFile
                ++ " "    ++ (builderConfigRuntime config </> "libddc-runtime.so")
        }


-- x86_32-cygwin ---------------------------------------------------------------
builder_X8632_Cygwin config
 =      Builder
        { builderName   = "x86_32-cygwin"
        , buildHost     = Platform ArchX86_32 OsCygwin
        , buildTarget   = Platform ArchX86_32 OsCygwin
        , buildSpec     = Llvm.platform32

        , buildLlc    
                = \llFile sFile
                -> doCmd "LLVM compiler" $ "llc -O3 -march=x86 " 
                ++ (normalise llFile)
                ++ " -o " ++ (normalise sFile)

        , buildAs
                = \sFile oFile
                -> doCmd "assembler" $  "as --32"  
                ++ " -o " ++ (normalise oFile)
                ++ " "    ++ (normalise sFile)

	-- Note on Cygwin we need to use 'gcc-4' explicitly because plain 'gcc'
	-- is a symlink, which Windows doesn't really support.
        , buildLdExe  
                = \oFile binFile
                -> doCmd "linker" $  "gcc-4 -m32" 
                ++ " -o " ++ (normalise binFile)
                ++ " "    ++ (normalise oFile)
                ++ " "    ++ (normalise $ builderConfigRuntime config </> "libddc-runtime.a")
        }


-- Utils ----------------------------------------------------------------------
-- | Run a system command, and if it fails quit the program.
doCmd :: String -> String -> IO ()
doCmd thing cmd
 = do   code <- system cmd
        case code of
         ExitSuccess    -> return ()
         ExitFailure _  
          -> error
                $ unlines
                [ "System command failed when invoking external " ++ thing ++ "."
                , " Command was: " ++ cmd ]

