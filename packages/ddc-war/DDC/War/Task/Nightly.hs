
-- | Run the nightly DDC build.
module DDC.War.Task.Nightly
        ( Spec   (..)
        , Result (..)
        , build)
where
import BuildBox.Command.System
import BuildBox.Command.File
import BuildBox.Pretty
import BuildBox
--import System.FilePath

data Spec
        = Spec
        { specRemoteSnapshotURL :: String
        , specRemoteRepoURL     :: String 
        , specLocalBuildDir     :: FilePath 
        , specRelPackageDir     :: String
        , specBuildThreads      :: Int }
        deriving Show


-- TODO: add log files to the ResultSuccess constructor
data Result
        = ResultSuccess


instance Pretty Result where
 ppr result
  = case result of
        ResultSuccess   -> text "success"


-- | Run the nightly DDC build.
build :: Spec -> Build Result
build (Spec urlSnapshot urlRepo dirBuild dirPackage buildThreads)
 = do   
        ensureDir dirBuild
        inDir     dirBuild
         $ do {- outLn "* Downloading snapshot"
                ssystem $ "wget --progress=bar " ++ urlSnapshot

                needs (takeFileName urlSnapshot)
                outLn "* Unpacking snapshot"
                ssystem $ "tar zxf " ++ takeFileName urlSnapshot
                -}
                inDir dirPackage
                 $ do{-   outLn "* Updating shapshot"
                        ssystem $ "darcs pull -av " ++ urlRepo

                        outLn "* Writing build config"
                        needs "make"
                        io $ writeFile "make/config-override.mk" 
                           $ unlines ["THREADS = " ++ show buildThreads]

                        outLn "* Building project"
                        ssystem $ "make nightly"
                     -}
                        outLn "* Building project"
                        ssystemTee True ("make totallogwar") ""
                        return ()

                        -- TODO: need to make controller keep running if we hit eof.


        return ResultSuccess

