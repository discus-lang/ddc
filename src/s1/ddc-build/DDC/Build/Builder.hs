
module DDC.Build.Builder
        ( BuilderConfig (..)
        , BuilderHost   (..)
        , Builder       (..)
        , BuilderResult (..)
        , builders

        , determineDefaultBuilder
        , determineDefaultBuilderHost)
where
import DDC.Build.Platform
import DDC.Data.Pretty                          hiding ((</>))
import Data.List
import System.FilePath
import System.Exit                              hiding (die)
import System.Process
import qualified DDC.Core.Salt.Platform         as Llvm


-- | Configuration information for a builder that is not platform specific.
data BuilderConfig
        = BuilderConfig
        { -- | Directory that holds the source for the runtime system
          --   and base library.
          builderConfigBaseSrcDir       :: FilePath

          -- | Directory that holds the shared objects for the runtime
          --   system and base library.
        , builderConfigBaseLibDir       :: FilePath

          -- | Runtime library to link with.
        , builderConfigLibFile          :: FilePath -> FilePath -> FilePath }


-- | Builder information that we determine by interrogating the host platform.
--   This tells us what we need to know about the environment that we're
--   building in, versions of software tools etc. This is separate
data BuilderHost
        = BuilderHost
        { builderHostLlvmVersion        :: String }


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


instance Show Builder where
 show builder
        = "Builder " ++ show (builderName builder)


instance Pretty Builder where
 ppr builder
        = vcat
        [ text "Builder Name : " <> text (builderName builder)
        , empty
        , text "Host Platform"
        , indent 1 $ ppr $ buildHost builder
        , empty
        , text "Target Platform"
        , indent 1 $ ppr $ buildTarget builder
        , empty
        , text "LLVM Target Spec"
        , indent 1 $ ppr $ buildSpec builder ]


-- builders -------------------------------------------------------------------
-- | All supported builders.
--   The host and target platforms are the same.
--
--   Supported builders are:
--      @x86_32-darwin@, @x86_64-darwin@,
--      @x86_32-linux@,  @x86_64-linux@,
--      @x86_32-cygwin@,
--      @ppc32-linux@
--
builders :: BuilderConfig -> BuilderHost -> [Builder]
builders config host
 =      [ builder_X8632_Darwin config host Nothing
        , builder_X8664_Darwin config host Nothing
        , builder_X8632_Linux  config host
        , builder_X8664_Linux  config host
        , builder_PPC32_Linux  config host ]


-- defaultBuilder -------------------------------------------------------------
-- | Determine the default builder based on the 'arch' and 'uname' commands.
--   This assumes that the 'host' and 'target' platforms are the same.
--
--   If we don't recognise the result of 'arch' or 'uname', or don't have
--   a default builder config for this platform then `Nothing`.
determineDefaultBuilder :: BuilderConfig -> IO (Maybe Builder)
determineDefaultBuilder config
 = do   mPlatform       <- determineHostPlatform
        mHost           <- determineDefaultBuilderHost

        case (mPlatform, mHost) of
         (Just (Platform ArchX86_32 (OsDarwin mVersion)), Just host)
                -> return $ Just (builder_X8632_Darwin config host mVersion)

         (Just (Platform ArchX86_64 (OsDarwin mVersion)), Just host)
                -> return $ Just (builder_X8664_Darwin config host mVersion)

         (Just (Platform ArchX86_32 OsLinux),  Just host)
                -> return $ Just (builder_X8632_Linux  config host)

         (Just (Platform ArchX86_64 OsLinux),  Just host)
                -> return $ Just (builder_X8664_Linux  config host)

         (Just (Platform ArchPPC_32 OsLinux),  Just host)
                -> return $ Just (builder_PPC32_Linux  config host)

         (Just (Platform ArchX86_32 OsCygwin), Just host)
                -> return $ Just (builder_X8632_Cygwin config host)

         (Just (Platform ArchX86_32 OsMingw),  Just host)
                -> return $ Just (builder_X8632_Mingw  config host)

         _      -> return Nothing


-- | Determine the default builder host configuration,
--   this the default set of build tools that we can see in the current path.
determineDefaultBuilderHost :: IO (Maybe BuilderHost)
determineDefaultBuilderHost
 = do
        -- Get the version of the LLVM suite in the current path.
        mStrLlvmVersion  <- determineHostLlvmVersion Nothing
        case mStrLlvmVersion of
         Nothing
          -> return Nothing

         Just strLlvmVersion
          -> return  $ Just $ BuilderHost
                     { builderHostLlvmVersion = strLlvmVersion }


-- x86_32-darwin ----------------------------------------------------------------
builder_X8632_Darwin config host mVersion
 =      Builder
        { builderName           = "x86_32-darwin"
        , buildHost             = Platform ArchX86_32 (OsDarwin mVersion)
        , buildTarget           = Platform ArchX86_32 (OsDarwin mVersion)
        , buildSpec             = Llvm.platform32
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "opt -O3"
                , llFile
                , "| llc -O3 -march=x86 -relocation-model=pic"
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "cc -Werror -std=c99 -O3 -m32"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "llvm-mc -arch x86 -filetype=obj"

                  -- From LLVM 3.8 we need to set the -triple explicitly which includes
                  -- the macosx OS specifier. llc inserts a pragma into the output
                  -- .s files saying they're for a specific version of macosx. If we
                  -- don't set the same version in the triple passed to llvm-mc then
                  -- it throws a warning. Note that Darwin v14.5 is OSX v10.10.5 etc.
                , case mVersion of
                        Nothing -> ""
                        Just (major, _minor, _patch)
                         -> "-triple=x86-apple-macosx10." ++ show (major - 4)

                , "-o", oFile
                ,       sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "cc -m32 -Wl,-dead_strip"
                , "-o", binFile
                , intercalate " " oFiles
                , builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> builderConfigLibFile config
                                "libddc-runtime.a"
                                "libddc-runtime.dylib" ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "cc -m32 -dynamiclib -undefined dynamic_lookup"
                  , "-o", libFile ] ++ oFiles
        }

-- x86_64-darwin --------------------------------------------------------------
builder_X8664_Darwin config host mVersion
 =      Builder
        { builderName           = "x86_64-darwin"
        , buildHost             = Platform ArchX86_64 (OsDarwin mVersion)
        , buildTarget           = Platform ArchX86_64 (OsDarwin mVersion)
        , buildSpec             = Llvm.platform64
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "opt -O3"
                , llFile
                , "| llc -O3 -march=x86-64 -relocation-model=pic"
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "cc -Werror -std=c99 -O3 -m64"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "llvm-mc -filetype=obj"

                  -- From LLVM 3.8 we need to set the -triple explicitly which includes
                  -- the macosx OS specifier. llc inserts a pragma into the output
                  -- .s files saying they're for a specific version of macosx. If we
                  -- don't set the same version in the triple passed to llvm-mc then
                  -- it throws a warning. Note that Darwin v14.5 is OSX v10.10.5 etc.
                , case mVersion of
                        Nothing -> ""
                        Just (major, _minor, _patch)
                         -> "-triple=x86_64-apple-macosx10." ++ show (major - 4)

                , "-o", oFile
                ,       sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                        [ "cc -m64 -Wl,-dead_strip"
                        , "-o", binFile
                        , intercalate " " oFiles
                        , builderConfigBaseLibDir config
                              </> "ddc-runtime" </> "build"
                              </> builderConfigLibFile config
                                      "libddc-runtime.a"
                                      "libddc-runtime.dylib" ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "cc -m64 -dynamiclib -undefined dynamic_lookup"
                  , "-o", libFile ] ++ oFiles
        }


-- x86_32-linux ---------------------------------------------------------------
builder_X8632_Linux config host
 =      Builder
        { builderName           = "x86_32-linux"
        , buildHost             = Platform ArchX86_32 OsLinux
        , buildTarget           = Platform ArchX86_32 OsLinux
        , buildSpec             = Llvm.platform32
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "llc -O3 -march=x86 -relocation-model=pic"
                ,       llFile
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -Wextra -pedantic -std=c99 -O3 -m32 -fPIC"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]


        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as --32"
                , "-o", oFile
                ,       sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "gcc -m32"
                , "-o", binFile
                , intercalate " " oFiles
                , builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> builderConfigLibFile config
                                "libddc-runtime.a"
                                "libddc-runtime.so"
                , "-lm" ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }


-- x86_64-linux ---------------------------------------------------------------
builder_X8664_Linux config host
 =      Builder
        { builderName           = "x86_64-linux"
        , buildHost             = Platform ArchX86_64 OsLinux
        , buildTarget           = Platform ArchX86_64 OsLinux
        , buildSpec             = Llvm.platform64
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "llc -O3 -march=x86-64 -relocation-model=pic"
                , llFile
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -std=c99 -O3 -m64 -fPIC"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]


        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as --64"
                , "-o", oFile
                , sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "gcc -m64"
                , "-o", binFile
                , intercalate " " oFiles
                , builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> builderConfigLibFile config
                                "libddc-runtime.a"
                                "libddc-runtime.so"
                , "-lm" ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }


-- ppc32-linux ---------------------------------------------------------------
builder_PPC32_Linux config host
 =      Builder
        { builderName           = "ppc32-linux"
        , buildHost             = Platform ArchPPC_32 OsLinux
        , buildTarget           = Platform ArchPPC_32 OsLinux
        , buildSpec             = Llvm.platform32
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "llc -O3 -march=ppc32 -relocation-model=pic"
                , llFile
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -std=c99 -O3 -m32"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as"
                , "-o", oFile
                , sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "gcc -m32"
                , "-o", binFile
                , intercalate " " $ map normalise oFiles
                , builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> builderConfigLibFile config
                                "libddc-runtime.a"
                                "libddc-runtime.so" ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }


-- x86_32-cygwin ---------------------------------------------------------------
builder_X8632_Cygwin config host
 =      Builder
        { builderName           = "x86_32-cygwin"
        , buildHost             = Platform ArchX86_32 OsCygwin
        , buildTarget           = Platform ArchX86_32 OsCygwin
        , buildSpec             = Llvm.platform32
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "llc -O3 -march=x86 "
                , normalise llFile
                , "-o", normalise sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc-4 -Werror -std=c99 -O3 -m32"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as --32"
                , "-o", normalise oFile
                , normalise sFile ]

    -- Note on Cygwin we need to use 'gcc-4' explicitly because plain 'gcc'
    -- is a symlink, which Windows doesn't really support.
        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "gcc-4 -m32"
                , "-o", normalise binFile
                , intercalate " " $ map normalise oFiles
                , normalise $ builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> "libddc-runtime.a" ]
                        -- configRuntimeLinkStrategy is ignored

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }


-- x86_32-mingw ----------------------------------------------------------------
builder_X8632_Mingw config host
 =      Builder
        { builderName           = "x86_32-mingw"
        , buildHost             = Platform ArchX86_32 OsMingw
        , buildTarget           = Platform ArchX86_32 OsMingw
        , buildSpec             = Llvm.platform32
        , buildBaseSrcDir       = builderConfigBaseSrcDir config
        , buildBaseLibDir       = builderConfigBaseLibDir config

        , buildLlvmVersion      = builderHostLlvmVersion  host
        , buildLlc
                = \llFile sFile
                -> doCmd "LLVM compiler"        [(2, BuilderCanceled)]
                [ "llc -O3 -march=x86 "
                , normalise llFile
                , "-o", normalise sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -std=c99 -O3 -m32"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/runtime"
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea/primitive" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as --32"
                , "-o", normalise oFile
                , normalise sFile ]

        , buildLdExe
                = \oFiles binFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                [ "gcc -m32"
                , "-o", normalise binFile
                , intercalate " " $ map normalise oFiles
                , normalise $ builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build"
                        </> "libddc-runtime.a" ]
                        -- configRuntimeLinkStrategy is ignored

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }


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

 where  cmd     = unwords cmdParts
        die c   = error
                $ unlines
                [ "System command failed when invoking external " ++ thing ++ "."
                , " Command was: " ++ cmd
                , " Exit code:   " ++ show c ]

