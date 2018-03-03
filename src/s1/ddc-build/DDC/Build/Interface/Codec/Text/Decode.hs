
module DDC.Build.Interface.Codec.Text.Decode where
import DDC.Build.Interface.Error
import DDC.Build.Interface.Base
import DDC.Core.Module
import DDC.Core.Transform.Reannotate
import Data.Time.Clock
import Control.Monad
import Data.Maybe
import qualified DDC.Core.Load                  as Load
import qualified DDC.Core.Discus                as Discus
import qualified DDC.Build.Language.Discus      as Discus
import qualified Data.Char                      as Char
import qualified Data.List                      as List


---------------------------------------------------------------------------------------------------
-- | Decode a textual interface from a string.
decodeInterface
        :: FilePath     -- ^ File path of interface file, for error messages.
        -> UTCTime      -- ^ TimeStamp of interface file.
        -> String       -- ^ Interface file source.
        -> Either Error InterfaceAA

decodeInterface pathInterface timeStamp str
 = let  -- Attach line numbers to ach line
        ls      = lines str
        lsNum   = zip [1..] ls
   in   pInterface pathInterface timeStamp lsNum


-- | Parse a atextual interface file.
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

                return  $ Interface
                        { interfaceFilePath     = pathInt
                        , interfaceTimeStamp    = timeStamp
                        , interfaceVersion      = version
                        , interfaceModuleName   = modName
                        , interfaceDiscusModule = liftM (reannotate (const ())) mTetra }

        | otherwise
        = Left $ ErrorBadMagic pathInt n


---------------------------------------------------------------------------------------------------
-- | A component of the interface file.
--   We use this as an intermediate form during parsing.
data Component
        = ComponentMeta
        { componentModuleName   :: ModuleName}

        | ComponentTetraModule
        { componentTetraModule  :: Module () Discus.Name }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Line numbers.
type LineNumber  = Int

-- | Parser for some thing.
type Parser a
        =  [(LineNumber, String)]
        -> Either Error a


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
        = case Load.parseModuleFromString Discus.fragment pathInt (n + 1)
                       (unlines $ map snd rest) of
                Left err   -> Left $ ErrorLoadTetra pathInt err
                Right m    -> return $ ComponentTetraModule $ reannotate (const ()) m

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


-- | Take the name from an interface tear line, if it is one.
takeInterfaceTearLine  :: String -> Maybe String
takeInterfaceTearLine str
        | '~' : '~' : ' ' : str2  <- str
        , (name, str3)            <- span (not . Char.isSpace) str2
        , (' ' : rest)            <- str3
        , all (== '~') rest
        = Just name

        | otherwise
        = Nothing

-- | Check if this string is an interface tear line.
isInterfaceTearLine :: String -> Bool
isInterfaceTearLine str
        = isJust $ takeInterfaceTearLine str

