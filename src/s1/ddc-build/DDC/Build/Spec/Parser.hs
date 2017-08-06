{-# OPTIONS_HADDOCK hide #-}
-- | Parser for DDC build spec files.
module DDC.Build.Spec.Parser
        ( parseBuildSpec
        , Error(..) )
where
import DDC.Build.Spec.Base
import DDC.Data.Pretty
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import qualified DDC.Core.Module        as C


---------------------------------------------------------------------------------------------------
-- | Problems that can arise when parsing a build spec file.
data Error
        -- | Empty Spec file.
        = ErrorEmpty
        { errorFilePath :: FilePath }

        -- | Parse error in Spec file.
        | ErrorParse
        { errorFilePath :: FilePath
        , errorLine     :: Int }

        -- | Required field is missing.
        | ErrorMissingField
        { errorFilePath :: FilePath
        , errorMissing  :: String }
        deriving Show


instance Pretty Error where
 ppr err
  = case err of
        ErrorEmpty filePath
         -> vcat [ text filePath
                 , text "Empty file" ]

        ErrorParse filePath n
         -> vcat [ text filePath <> text ":" <> int n
                 , text "Parse error" ]

        ErrorMissingField filePath field
         -> vcat [ text filePath
                 , text "Missing field '" <> text field <> text "'" ]


---------------------------------------------------------------------------------------------------
type LineNumber = Int
type StartCol   = Int

type Parser a   = [(LineNumber, StartCol, String)]
                -> Either Error a


-- | Parse a build specification.
parseBuildSpec :: FilePath -> String -> Either Error Spec
parseBuildSpec path str
 = let  -- Attach line numbers and starting column to each line.
        ls              = lines str
        lsNum           = zip [1..]  ls
        lsNumCols       = attachCols lsNum
   in   pBuildSpec path lsNumCols


-- | Parse a build specification.
pBuildSpec :: FilePath -> Parser Spec
pBuildSpec path []
        = Left $ ErrorEmpty path

pBuildSpec path ((n, _s, str) : rest)
        -- Skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pBuildSpec path rest

        -- The build spec needs to start with the magic words and version number.
        | ["ddc", "build", version] <- words str
        = do    cs    <- pComponents path rest
                return  $ Spec
                        { specVersion           = version
                        , specComponents        = cs }

        | otherwise
        = Left $ ErrorParse path n


-- | Parse a build component specification.
pComponents :: FilePath -> Parser [Component]
pComponents _path []
        = return []

pComponents path ((n, start, str) : rest)
        -- skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pComponents path rest

        -- parse a library specification
        | str == "library"
        , (lsLibrary, lsMore)
                <- span (\(_, start', _) -> start' == 0 || start' > start) rest
        = do    fs      <- pLibraryFields path lsLibrary
                more    <- pComponents    path lsMore
                return  $ fs : more

        -- parse an executable specification
        | str == "executable"
        , (lsExecutable, lsMore)
                <- span (\(_, start', _) -> start' == 0 || start' > start) rest
        = do    fs      <- pExecutableFields path lsExecutable
                more    <- pComponents       path lsMore
                return  $ fs : more

        | otherwise
        = Left $ ErrorParse path n


---------------------------------------------------------------------------------------------------
-- | Parse the fields of a library specification.
pLibraryFields :: FilePath -> Parser Component
pLibraryFields path str
 = do   fs                          <- pFields path str
        (sName,         fs_name)    <- takeField path "name"             fs
        (sVersion,      fs_version) <- takeField path "version"          fs_name
        (sTetraModules, fs_modules) <- takeField path "tetra-modules"    fs_version

        let Just msTetra
                = sequence
                $ map C.readModuleName
                $ words $ sTetraModules

        return  $ SpecLibrary
                { specLibraryName          = sName
                , specLibraryVersion       = sVersion
                , specLibraryTetraModules  = msTetra
                , specLibraryMeta          = fs_modules }


---------------------------------------------------------------------------------------------------
-- | Parse the fields of an executable specification.
pExecutableFields :: FilePath -> Parser Component
pExecutableFields path str
 = do   fs                          <- pFields path str
        (sName,           fs_name)  <- takeField      path "name"        fs
        (sTetraMain,      fs_main)  <- takeField      path "tetra-main"  fs_name
        let (sTetraOther, fs_other) =  takeFieldMaybe path "tetra-other" fs_main

        let Just mTetraMain
                = C.readModuleName sTetraMain

        let Just msTetra
                = sequence $ map C.readModuleName
                $ concat   $ maybeToList $ liftM words sTetraOther

        return  $ SpecExecutable
                { specExecutableName       = sName
                , specExecutableTetraMain  = mTetraMain
                , specExecutableTetraOther = msTetra
                , specExecutableMeta       = fs_other }


---------------------------------------------------------------------------------------------------
-- | Parse fields of a build specification.
pFields  :: FilePath -> Parser [(String, String)]
pFields _path []
        = return []

pFields path ((n, start, str) : rest)
        -- skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pFields path rest

        -- parse a single field.
        | (lsField, lsMore)
                <- span (\(_, start', _) -> start' == 0 || start' > start) rest

        , (fieldName, ':' : fieldValue)
                <- span (\c -> c /= ':')
                $  str ++ concat [ s | (_, _, s) <- lsField]

        = do    let f   =  (chomp fieldName, chomp fieldValue)
                more    <- pFields path lsMore
                return  $ f : more

        | otherwise
        = Left $ ErrorParse path n


-- | Take a named field from this list of fields.
takeField :: FilePath
          -> String -> [(String, String)]
          -> Either Error (String, [(String, String)])

takeField path name fs
 = case lookup name fs of
        Nothing -> Left $ ErrorMissingField path name
        Just s  -> return (s, delete (name, s) fs)


-- | Take a named field from this list of fields.
takeFieldMaybe
        :: FilePath
        -> String -> [(String, String)]
        -> (Maybe String, [(String, String)])

takeFieldMaybe _path name fs
 = case lookup name fs of
        Nothing -> (Nothing, fs)
        Just s  -> (Just s,  delete (name, s) fs)


---------------------------------------------------------------------------------------------------
-- | Attach starting column number to these lines.
attachCols
        :: [(LineNumber, String)]
        -> [(LineNumber, StartCol, String)]

attachCols lstrs
 = [ (ln, startCol 1 str, str) | (ln, str) <- lstrs ]
 where  startCol n ss
         = case ss of
                []              -> 0
                ' '  : ss'      -> startCol (n + 1) ss'
                '\t' : ss'      -> startCol (n + 8) ss'
                _    : _        -> n


-- | Remove whitespace from the beginning and end of a string.
chomp :: String -> String
chomp str
        = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

