module DDC.Build.Builder.BuilderPPC32Linux where
import DDC.Build.Builder.Base
import qualified DDC.Core.Salt.Platform as Llvm


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
                [ builderHostLlvmBinPath host </> "llc"
                , "-O3 -march=ppc32 -relocation-model=pic"
                , llFile
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -std=c99 -O3 -m32"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea" ]

        , buildAs
                = \sFile oFile
                -> doCmd "assembler"            [(2, BuilderCanceled)]
                [ "as"
                , "-o", oFile
                , sFile ]

        , buildLdExe
           = \oFiles binFile
           -> do let pathRuntime
                        = builderConfigBaseLibDir config
                        </> "ddc-runtime" </> "build" </> "libddc-runtime"
                        <.> (if builderConfigLinkStatic config then ".a" else ".so")

                 doCmd "linker"                 [(2, BuilderCanceled)]
                       [ "gcc -m32"
                       , "-o", binFile
                       , intercalate " " $ map normalise oFiles
                       , pathRuntime ]

        , buildLdLibStatic
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
                = \oFiles libFile
                -> doCmd "linker"               [(2, BuilderCanceled)]
                $ [ "gcc -shared", "-o", libFile ] ++ oFiles
        }
