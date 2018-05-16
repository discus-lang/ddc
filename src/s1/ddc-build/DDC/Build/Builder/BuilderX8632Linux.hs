module DDC.Build.Builder.BuilderX8632Linux where
import DDC.Build.Builder.Base
import qualified DDC.Core.Salt.Platform as Llvm

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
                [ builderHostLlvmBinPath host </> "llc"
                , "-O3 -march=x86 -relocation-model=pic"
                ,       llFile
                , "-o", sFile ]

        , buildCC
                = \cFile oFile
                -> doCmd "C compiler"           [(2, BuilderCanceled)]
                [ "gcc -Werror -Wextra -pedantic -std=c99 -O3 -m32 -fPIC"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea" ]


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
