
module DDC.War.Create.CreateDCE
        (create)
where
import DDC.War.Job


-- Create ---------------------------------------------------------------------
create :: FilePath -> Maybe Chain
create file
 | isSuffixOf ".dce"
 = let  
        mainSH           = sourceDir </> "Main.sh"
        mainBin          = buildDir  </> "Main.bin"
        mainCompStdout   = buildDir  </> "Main.compile.stdout"
        mainCompStderr   = buildDir  </> "Main.compile.stderr"
        mainCompDiff     = buildDir  </> "Main.compile.stderr.diff"
        mainRunStdout    = buildDir  </> "Main.run.stdout"
        mainRunStderr    = buildDir  </> "Main.run.stderr"

        mainErrorCheck   = sourceDir </> "Main.error.check"
        shouldSucceed    = not $ Set.member mainErrorCheck allFiles

        mainStdoutCheck  = sourceDir </> "Main.stdout.check"
        mainStdoutDiff   = buildDir  </> "Main.run.stdout.diff"
        shouldDiffStdout = Set.member mainStdoutCheck allFiles

        mainStderrCheck  = sourceDir </> "Main.stderr.check"
        mainStderrDiff   = buildDir  </> "Main.run.stderr.diff"
        shouldDiffStderr = Set.member mainStderrCheck allFiles

        -- compile the .ds into a .bin
        compile         = Job   $ CompileDCE.build $ CompileDCE.Spec
                                testName (wayName way) filePath
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) shouldSucceed

        -- run the binary
        run             = Job   $ RunExe.build $ RunExe.Spec
                                testName (wayName way) filePath mainBin
                                mainRunStdout mainRunStderr

        -- diff errors produced by the compilation
        diffError      = Job    $ RunDiff.build $ RunDiff.Spec
                                testName (wayName way) mainErrorCheck
                                mainCompStderr mainCompDiff

        -- diff the stdout of the run
        diffStdout     = Job    $ RunDiff.build $ RunDiff.Spec
                                testName (wayName way) mainStdoutCheck
                                mainRunStdout mainStdoutDiff

        -- diff the stderr of the run
        diffStderr     = Job    $ RunDiff.build $ RunDiff.spec
                                testName (wayName way) mainStderrCheck
                                mainRunStderr mainStderrDiff

   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain 
                $ [compile]
                ++ (if shouldSucceed    then [run]        else [diffError])
                ++ (if shouldDiffStdout then [diffStdout] else [])
                ++ (if shouldDiffStderr then [diffStderr] else [])

