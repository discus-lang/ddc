
-- | Loader for DDC interface files.
module DDC.Build.Interface.Load
        ( loadInterface
        , Error (..)
        , InterfaceAA)
where
import DDC.Build.Interface.Base
import DDC.Core.Module
import DDC.Data.Pretty
import DDC.Core.Transform.Reannotate
import Data.Time.Clock
import Control.Monad
import qualified DDC.Core.Load                  as Load
import qualified DDC.Core.Tetra                 as Tetra
import qualified DDC.Build.Language.Tetra       as Tetra
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Build.Language.Salt        as Salt
import qualified Data.Char                      as Char
import qualified Data.List                      as List


---------------------------------------------------------------------------------------------------
-- | Problems that can arise when loading an interface file.
data Error
        -- | Empty Interface file.
        = ErrorEmpty

        -- | No module meta information.
        | ErrorNoMeta

        -- | Duplicate module information.
        | ErrorDuplicate

        -- | Bad magic numbers / header information in alleged interface file.
        --   This probably isn't an interface file.
        | ErrorBadMagic
        { errorFilePath :: FilePath
        , errorLine     :: Int }

        -- | Parse error in Interface file.
        | ErrorParse
        { errorFilePath :: FilePath
        , errorLine     :: Int}

        -- | Parser error at end of input.
        | ErrorParseEnd

        -- | Error when loading a tetra core module from the interface file.
        | ErrorLoadTetra FilePath (Load.Error Tetra.Name Tetra.Error)

        -- | Error when loading a salt  core module from the interface file.
        | ErrorLoadSalt  FilePath (Load.Error  Salt.Name  Salt.Error)


instance Pretty Error where
 ppr ErrorEmpty
  = vcat [ text "Empty interface file." ]

 ppr ErrorNoMeta
  = vcat [ text "No metadata section in interface file." ]

 ppr ErrorDuplicate
  = vcat [ text "Duplicate section in interface file." ]

 ppr (ErrorBadMagic path l)
  = vcat [ text path <> text ":" <> int l
         , text "Bad header in interface file." ]

 ppr (ErrorParse path l)
  = vcat [ text path <> text ":" <> int l
         , text "Parse error in interface file." ]

 ppr ErrorParseEnd
  = vcat [ text "Parse error at end of interface file." ]

 ppr (ErrorLoadTetra path err)
  = vcat [ text path
         , text "Error when loading Tetra module from interface file."
         , indent 2 $ ppr err ]

 ppr (ErrorLoadSalt path err)
  = vcat [ text path
         , text "Error when loading Salt module from interface file."
         , indent 2 $ ppr err ]


---------------------------------------------------------------------------------------------------
-- | Line numbers.
type LineNumber  = Int

-- | Parser for some thing.
type Parser a
        =  [(LineNumber, String)]
        -> Either Error a

-- | Type of annotated interface.
--   As don't store full Salt code in interface files,
--   we just set the annotation for it to ()
type InterfaceAA
        = Interface () ()


---------------------------------------------------------------------------------------------------
-- | Load an interface file.
loadInterface
        :: FilePath     -- ^ File path of interface file, for error messages.
        -> UTCTime      -- ^ TimeStamp of interface file.
        -> String       -- ^ Interface file source.
        -> Either Error InterfaceAA

loadInterface pathInterface timeStamp str
 = let  -- Attach line numbers to ach line
        ls      = lines str
        lsNum   = zip [1..] ls
   in   pInterface pathInterface timeStamp lsNum


-- | Parse an interface file.
pInterface
        :: FilePath             -- ^ Path of interface file.
        -> UTCTime              -- ^ TimeStamp of interface file.
        -> Parser InterfaceAA

pInterface _pathInt _timeStamp []
        = Left ErrorEmpty

pInterface pathInt timeStamp ((n, str) : rest)
        -- Skip over blank lines
        | all (\c -> Char.isSpace c || c == '\n') str
        = pInterface pathInt timeStamp rest

        -- The interface needs to start with the magic words and version number.
        | ["ddc", "interface", version] <- words str
        = do    cs              <- pComponents pathInt rest

                -- We need exactly one module meta-data component.
                modName <- case [m | m@ComponentMeta{} <- cs] of
                                [m]     -> return $ componentModuleName m
                                _       -> Left $ ErrorNoMeta

                -- Accept a tetra module in the interface file.
                mTetra  <- case [m | m@ComponentTetraModule{} <- cs] of
                                []      -> return Nothing
                                [m]     -> return $ Just $ componentTetraModule m
                                _       -> Left ErrorDuplicate

                -- Accept a salt module in the interface file.
                mSalt   <- case [m | m@ComponentSaltModule{}  <- cs] of
                                []      -> return Nothing
                                [m]     -> return $ Just $ componentSaltModule m
                                _       -> Left ErrorDuplicate

                return  $ Interface
                        { interfaceFilePath     = pathInt
                        , interfaceTimeStamp    = timeStamp
                        , interfaceVersion      = version
                        , interfaceModuleName   = modName
                        , interfaceTetraModule  = liftM (reannotate (const ())) mTetra
                        , interfaceSaltModule   = liftM (reannotate (const ())) mSalt }

        | otherwise
        = Left $ ErrorBadMagic pathInt n


---------------------------------------------------------------------------------------------------
-- | A component of the interface file.
--   We use this as an intermediate form during parsing.
data Component
        = ComponentMeta
        { componentModuleName   :: ModuleName}

        | ComponentTetraModule
        { componentTetraModule  :: Module () Tetra.Name }

        | ComponentSaltModule
        { componentSaltModule   :: Module () Salt.Name  }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Parse some components of an interface file.
pComponents :: FilePath -> Parser [Component]
pComponents _ []
        = return []

pComponents pathInterface (l : ls)
        -- skip blank lines
        | all Char.isSpace (snd l)
        = pComponents pathInterface ls

        -- parse a single section
        | isInterfaceTearLine (snd l)
        = do   let (ls', rest) = List.break (isInterfaceTearLine . snd) ls
               c       <- pComponent  pathInterface (l : ls')
               cs      <- pComponents pathInterface rest
               return  $ c : cs

        | otherwise
        = Left $ ErrorParse pathInterface (fst l)


---------------------------------------------------------------------------------------------------
-- | Parse a single component of an interface file.
pComponent :: FilePath -> Parser Component
pComponent _ []
 = Left $ ErrorParseEnd

pComponent pathInt ((n, l) : rest)
        -- skip blank lines
        | all Char.isSpace l
        = pComponent pathInt rest

        -- load a module meta-data section.
        | Just "Meta"  <- takeInterfaceTearLine l
        = pComponentMeta pathInt rest

        -- load a Tetra core module section.
        | Just "Tetra" <- takeInterfaceTearLine l
        = case Load.parseModuleFromString Tetra.fragment pathInt (n + 1)
                       (unlines $ map snd rest) of
                Left err   -> Left $ ErrorLoadTetra pathInt err
                Right m    -> return $ ComponentTetraModule $ reannotate (const ()) m

        -- load a Salt core module section.
        | Just "Salt"  <- takeInterfaceTearLine l
        = case Load.parseModuleFromString Salt.fragment pathInt (n + 1)
                       (unlines $ map snd rest) of
               Left err   -> Left $ ErrorLoadSalt pathInt err
               Right m    -> return $ ComponentSaltModule  $ reannotate (const ()) m

        -- this thing didn't parse.
        | otherwise
        = Left $ ErrorParse pathInt n


---------------------------------------------------------------------------------------------------
-- | Parse module meta data from an interface file.
pComponentMeta :: FilePath -> Parser Component
pComponentMeta _pathInt []
        = Left ErrorParseEnd

pComponentMeta pathInt nls@((n, _) : _)
        | "module-meta" : "{" : "name:" : strModName : "}" : []
                <- tokenize $ concatMap snd nls
        , Just modName     <- moduleNameOfString strModName
        = return $ ComponentMeta
                 { componentModuleName   = modName }

        | otherwise
        = Left   $ ErrorParse pathInt n


---------------------------------------------------------------------------------------------------
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

