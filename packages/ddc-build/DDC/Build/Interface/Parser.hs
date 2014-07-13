
-- | Parser for DDC interface files.
module DDC.Build.Interface.Parser
        ( parseInterface
        , Error (..))
where
import DDC.Build.Interface.Base
import DDC.Core.Check                           (AnTEC)
import DDC.Core.Module
import DDC.Base.Pretty
import qualified DDC.Core.Load                  as Load
import qualified DDC.Core.Tetra                 as Tetra
import qualified DDC.Build.Language.Tetra       as Tetra
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Data.SourcePos             as BP
import qualified Data.Char                      as Char
import qualified Data.List                      as List


---------------------------------------------------------------------------------------------------
-- | Problems that can arise when parsing an interface file.
data Error
        -- | Empty Interface file.
        = ErrorEmpty

        -- | No module meta information.
        | ErrorNoMeta

        -- | Duplicate module information.
        | ErrorDuplicateCore

        -- | Parse error in Interface file.
        | ErrorParse
        { errorLine     :: Int}

        -- | Parser error at end of input.
        | ErrorParseEnd
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Line numbers.
type LineNumber  = Int

-- | Parser for some thing.
type Parser a    
        =  [(LineNumber, String)]
        -> Either Error a

-- | Type of annotated interface.
type InterfaceAA 
        = Interface (AnTEC BP.SourcePos Tetra.Name) 
                    (AnTEC BP.SourcePos Salt.Name)


-- | Parse an interface file.
parseInterface 
        :: FilePath     -- ^ File path of interface file, for error messages.
        -> String       -- ^ Interface file source.
        -> Either Error 
                  (Interface (AnTEC BP.SourcePos Tetra.Name) 
                             (AnTEC BP.SourcePos Salt.Name))

parseInterface pathInterface str
 = let  -- Attach line numbers to ach line
        ls      = lines str
        lsNum   = zip [1..] ls
   in   pInterface pathInterface lsNum


-- | Parse an interface file.
pInterface :: FilePath -> Parser InterfaceAA
pInterface _pathInterface []
        = Left ErrorEmpty

pInterface pathInterface ((n, str) : rest)
        -- Skip over blank lines
        | all (\c -> Char.isSpace c || c == '\n') str
        = pInterface pathInterface rest

        -- The interface needs to start with the magic words and version number.
        | ["ddc", "interface", version] <- words str
        = do    cs              <- pComponents pathInterface rest

                -- We need exactly one module meta-data component.
                modName <- case [m | m@ComponentMeta{} <- cs] of
                                [m]     -> return $ componentModuleName m
                                _       -> Left $ ErrorNoMeta

                -- Accept a tetra module in the interface file.
                mTetra  <- case [m | m@ComponentTetraModule{} <- cs] of
                                []      -> return Nothing
                                [m]     -> return $ Just $ componentTetraModule m
                                _       -> Left ErrorDuplicateCore

                -- Accept a salt module in the interface file.
                mSalt   <- case [m | m@ComponentSaltModule{}  <- cs] of
                                []      -> return Nothing
                                [m]     -> return $ Just $ componentSaltModule m
                                _       -> Left ErrorDuplicateCore

                return  $ Interface
                        { interfaceVersion      = version
                        , interfaceModuleName   = modName
                        , interfaceTetraModule  = mTetra
                        , interfaceSaltModule   = mSalt }

        | otherwise
        = Left $ ErrorParse n


---------------------------------------------------------------------------------------------------
-- | A component of the interface file. 
--   We use this as an intermediate form during parsing.
data Component
        = ComponentMeta
        { componentModuleName   :: ModuleName}

        | ComponentTetraModule
        { componentTetraModule  :: Module (AnTEC BP.SourcePos Tetra.Name) Tetra.Name }

        | ComponentSaltModule
        { componentSaltModule   :: Module (AnTEC BP.SourcePos Salt.Name)  Salt.Name  }
        deriving Show


-- | Parse some components of an interface file.
pComponents :: FilePath -> Parser [Component]
pComponents _ []
        = return []

pComponents pathInterface ls
 = do   let (ls', rest) 
                = List.break
                        (\(_, l) -> l == interfaceTearLine) ls

        c       <- pComponent  pathInterface ls'

        cs      <- pComponents pathInterface 
                        (dropWhile (\(_, l) -> l == interfaceTearLine) rest)

        return  $ c : cs


-- | Parse a single component of an interface file.
pComponent :: FilePath -> Parser Component
pComponent _ []   
 = Left $ ErrorParseEnd

pComponent pathInterface nls@((n, _) : _)

 = case words $ concatMap snd nls of
    "module-meta" : _         
      -> pComponentMeta nls

    "tetra" : sm@"module" : rest
      -> case Load.loadModuleFromString Tetra.fragment pathInterface n 
                Load.Recon (unwords $ sm : rest) of
           (Left err, _)  -> error  $ renderIndent $ ppr err
           (Right m,  _)  -> return $ ComponentTetraModule m

    "salt"  : sm@"module" : rest    
      -> case Load.loadModuleFromString Salt.fragment pathInterface n
                Load.Recon (unwords $ sm : rest) of
           (Left err, _)  -> error  $ renderIndent $ ppr err
           (Right m,  _)  -> return $ ComponentSaltModule  m

    _ -> Left $ ErrorParse n


-- | Parse module meta data from an interface file.
pComponentMeta :: Parser Component
pComponentMeta [] 
        = Left ErrorParseEnd

pComponentMeta nls@((n, _) : _)
        | "module-meta" : "{" : "name:" : strModName : "}" : []
                <- tokenize $ concatMap snd nls
        , Just modName     <- moduleNameOfString strModName
        = return $ ComponentMeta 
                 { componentModuleName   = modName }

        | otherwise
        = Left   $ ErrorParse n


-- | Tokenise the interface header containing module meta data.
tokenize :: String -> [String]
tokenize str
 = go [] str
 where  go acc []         = pop acc []

        go acc (c : cs)
         | Char.isSpace c = pop acc (go [] cs)
         | c == '{'       = pop acc ("{" : go [] cs)
         | c == '}'       = pop acc ("}" : go [] cs)
         | otherwise      = go (c : acc) cs

        pop acc x
         = case acc of
                []      -> x
                _       -> reverse acc : x


-- | Parse a String as a `ModuleName`, or Nothing if it isn't one.
moduleNameOfString :: String -> Maybe ModuleName
moduleNameOfString str
 = Just $ ModuleName $ go str
 where
        go s
         | elem '.' s
         , (n, '.' : rest) <- span (/= '.') s
         = n : go rest

         | otherwise
         = [s]

