{-# OPTIONS_HADDOCK hide #-}

module DDC.Build.Builder.BuilderX8664Darwin where
import DDC.Build.Builder.Base
import qualified DDC.Core.Salt.Platform as Llvm
import qualified System.Directory       as System


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
           -> doCmd "LLVM compiler"     [(2, BuilderCanceled)]
                [ builderHostLlvmBinPath host </> "opt"
                , llFile
                , "|"
                , builderHostLlvmBinPath host </> "llc"
                , "-O1 -march=x86-64 -relocation-model=pic"
                , "-o", sFile ]

        , buildCC
           = \cFile oFile
           -> doCmd "C compiler"        [(2, BuilderCanceled)]
                [ "cc -Werror -std=c99 -O3 -m64"
                , "-c", cFile
                , "-o", oFile
                , "-I" ++ builderConfigBaseSrcDir config </> "ddc-runtime/sea" ]

        , buildAs
           = \sFile oFile
           -> doCmd "assembler"         [(2, BuilderCanceled)]
                [ builderHostLlvmBinPath host </> "llvm-mc"
                , "-filetype=obj"

                  -- From LLVM 3.8 we need to set the -triple explicitly which includes
                  --   the macosx OS specifier. llc inserts a pragma into the output
                  --   .s files saying they're for a specific version of macosx. If we
                  --   don't set the same version in the triple passed to llvm-mc then
                  --   it throws a warning. Note that Darwin v14.5 is OSX v10.10.5 etc.
                , case mVersion of
                        Nothing -> ""
                        Just (major, _minor, _patch)
                         -> "-triple=x86_64-apple-macosx10." ++ show (major - 4)

                , "-o", oFile
                ,       sFile ]

        , buildLdExe
           = \oFiles binFile -> do
                let pathBuild   =   builderConfigBaseLibDir config
                                </> "ddc-runtime" </> "build"

                let pathRuntime =   pathBuild </> "libddc-runtime"
                                <.> (if builderConfigLinkStatic config
                                        then "a" else "dylib")

                doCmd "linker"          [(2, BuilderCanceled)]
                 [ "cc -m64"
                 , "-Wl,-dead_strip"
                 , "-o", binFile
                 , intercalate " " oFiles
                 , pathRuntime ]

        , buildLdLibStatic
           = \oFiles libFile
           -> doCmd "linker"            [(2, BuilderCanceled)]
                $ ["ar r", libFile] ++ oFiles

        , buildLdLibShared
           = \oFiles libFile -> do
                -- The install_name is the intended install path of the dylib.
                --  When executables are linked against a library with an install_name
                --  set that path is added to the linker meta-data of the executable.
                --  When the system then loads the executable it tries to find the dylib
                --  at that previously set path.
                --
                -- Setting headerpad_max_install_names adds space to the header so
                --   that the install_name can be rewritten to a longer one using
                --   the command line install_name_tool.
                --
                libFile'  <- System.makeAbsolute libFile

                doCmd "linker"          [(2, BuilderCanceled)]
                 $ [ "cc -m64"
                   , "-dynamiclib -undefined dynamic_lookup"
                   , "-install_name " ++ libFile'
                   , "-headerpad_max_install_names"
                   , "-o", libFile ] ++ oFiles
        }

