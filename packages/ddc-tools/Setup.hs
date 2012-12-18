import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import System.Exit
import System.Cmd

main    = defaultMainWithHooks hooks

hooks   = simpleUserHooks
        { postInst      = hookPostInstall }


-- Build the base library from the ddc-code package after we've installed ddc.
hookPostInstall 
        :: Args
        -> InstallFlags
        -> PackageDescription
        -> LocalBuildInfo
        -> IO ()

hookPostInstall _args _flags desc buildInfo
 = do   putStrLn "Building base libraries..."

        -- Find out where Cabal has put the DDC executable.
        let installDirs = absoluteInstallDirs desc buildInfo NoCopyDest
        let ddcExe      =   (bindir $ installDirs)
                        </> (fromPathTemplate $ progPrefix buildInfo)
                        ++  "ddc" 
                        <.> (fromPathTemplate $ progSuffix buildInfo)

        -- Run the basebuild command, which builds the library.
        code            <- system $ ddcExe ++ " -basebuild -O"
        case code of
         ExitFailure _  -> error "Failed!"
         ExitSuccess    -> return ()
