
module DDC.Build.Spec.Parser
        ( Error(..)
        , parseBuildSpec)
where
import DDC.Build.Spec.Base
import Data.List
import Data.Char


-- | Problems that can arise when parsing a build spec file.
data Error 
        -- | Empty Spec file.
        = ErrorEmpty

        -- | Parse error in Spec file.
        | ErrorParse
        { errorLine     :: Int }

        -- | Required field is missing.
        | ErrorMissingField
        { errorMissing  :: String }
        deriving Show


---------------------------------------------------------------------------------------------------
type LineNumber = Int
type StartCol   = Int

type Parser a   = [(LineNumber, StartCol, String)]  
                -> Either Error a


-- | Parse a build specification.
parseBuildSpec :: String -> Either Error Spec
parseBuildSpec str
 = let  -- Attach line numbers and starting column to each line.
        ls              = lines str
        lsNum           = zip [1..]  ls
        lsNumCols       = attachCols lsNum
   in   pBuildSpec lsNumCols


-- | Parse a build specification.
pBuildSpec :: Parser Spec
pBuildSpec  []
        = Left $ ErrorEmpty

pBuildSpec ((n, _s, str) : rest)
        -- skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pBuildSpec rest

        -- The build spec needs to start with the magic words and version number.
        | ["ddc", "build", version]     <- words str
        = do    cs    <- pComponents rest
                return  $ Spec
                        { specVersion           = version
                        , specComponents        = cs }

        | otherwise
        = Left $ ErrorParse n


-- | Parse a build component specification.
pComponents :: Parser [Component]
pComponents []
        = return []

pComponents ((n, start, str) : rest)
        -- skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pComponents rest

        -- parse a library specification
        | isPrefixOf str "library"
        , (lsLibrary, lsMore) 
                <- span (\(_, start', _) -> start' == 0 || start' > start) rest
        = do    lib     <- pLibraryFields    lsLibrary
                more    <- pComponents lsMore
                return  $ lib : more

        | otherwise
        = Left $ ErrorParse n


-- | Parse the fields of a library specification.
pLibraryFields :: Parser Component
pLibraryFields str
 = do   fs                           <- pFields str
        (sName,         fs_name)     <- takeField "name"          fs
        (sVersion,      fs_version)  <- takeField "version"       fs_name
        (sTetraModules, fs_modules)  <- takeField "tetra-modules" fs_version

        return  $ SpecLibrary 
                { specLibraryName               = sName
                , specLibraryVersion            = sVersion
                , specLibraryTetraModules       = words $ sTetraModules
                , specLibraryMeta               = fs_modules }


-- | Take a named field from this list of fields.
takeField :: String -> [(String, String)] 
          -> Either Error (String, [(String, String)])
takeField name fs
 = case lookup name fs of
        Nothing -> Left $ ErrorMissingField name
        Just s  -> return (s, delete (name, s) fs)


-- | Parse fields of a build specification.
pFields  :: Parser [(String, String)]
pFields []
        = return []

pFields ((n, start, str) : rest)
        -- skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pFields rest
        
        -- parse a single field.
        | (lsField, lsMore)      
                <- span (\(_, start', _) -> start' == 0 || start' > start) rest

        , (fieldName, ':' : fieldValue) 
                <- span (\c -> c /= ':') 
                $  str ++ concat [ s | (_, _, s) <- lsField]

        = do    let f   =  (chomp fieldName, chomp fieldValue)
                more    <- pFields lsMore
                return  $ f : more

        | otherwise
        = Left $ ErrorParse n


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

