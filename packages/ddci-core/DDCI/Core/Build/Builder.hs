
module DDCI.Core.Build.Builder
        ( Builder (..)
        , builder_I386_Darwin)
where
import System.Cmd


data Builder
        = Builder
        { buildLlc      :: FilePath -> FilePath -> IO Bool 
        , buildAs       :: FilePath -> FilePath -> IO Bool 
        , buildLdExe    :: FilePath -> FilePath -> IO Bool }

builder_I386_Darwin
 = let  buildLlc' llFile sFile
         = doCmd $ "llc -O3 -march=x86 " 
                 ++ llFile 
                 ++ " -o " ++ sFile

        buildAs' sFile oFile
         = doCmd $  "as -arch i386"  
                 ++ " -o " ++ oFile
                 ++ " "    ++ sFile  

        buildLdExe' oFile binFile
         = doCmd $  "gcc -m32" 
                 ++ " -o " ++ binFile
                 ++ " "    ++ oFile
                 ++ " "    ++ "packages/ddc-core-sea/runtime/Primitive.o"

        doCmd cmd
         = do   _ <- system cmd
                return True

   in   Builder buildLlc' buildAs' buildLdExe'

