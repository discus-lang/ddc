{-# OPTIONS_HADDOCK hide #-}
module DDC.Build.Spec.Base
        ( Spec          (..)
        , Component     (..)
        , specFieldsLibrary)
where
import DDC.Core.Module  (ModuleName)

-- | A build specification.
--   This specifies how to build a particular library or exectutable,
--   and is equivalent to a Haskell Cabal file.
data Spec
        = Spec
        { -- | Version number set by the first line of the file.
          specVersion                   :: String

          -- | Components in this build specification.
        , specComponents                :: [Component] }
        deriving Show


-- | A build component.
data Component
        = SpecLibrary
        { -- | Name of the library.
          specLibraryName               :: String

          -- | Library version.
        , specLibraryVersion            :: String

          -- | Tetra modules to build, in dependency order.
        , specLibraryTetraModules       :: [ModuleName]

          -- | Optional library meta-data.
          --   These fields are for informational purposes and are not
          --   nessesary to build the library itself.
        , specLibraryMeta               :: [(String, String)] }


        | SpecExecutable
        { -- | Name of executable
          specExecutableName            :: String

          -- | Name of main module.
        , specExecutableTetraMain       :: ModuleName

          -- | Tetra modules to build, in dependency order.
        , specExecutableTetraOther      :: [ModuleName]

          -- | Optional library meta-data.
          --   These fields are for informational purposes and are not
          --   nessesary to build the executable itself.
        , specExecutableMeta            :: [(String, String)] }
        deriving Show


-------------------------------------------------------------------------------

-- | Names of all allowable fields in library metadata, and whether each
--   field is nessesary or optional.
specFieldsLibrary :: [(String, Bool)]
specFieldsLibrary
 =  [ (str, True)  | str <-
        [ "name"
        , "version"
        , "tetra-modules" ]]

 ++ [ (str, False) | str <-
        [ "author"
        , "maintainer"
        , "homepage"
        , "license"
        , "synopsis" ]]
