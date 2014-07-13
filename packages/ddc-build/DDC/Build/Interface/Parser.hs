
module DDC.Build.Interface.Parser
        ( parseInterface
        , Error (..))
where
import DDC.Build.Interface.Base
import DDC.Core.Check                   (AnTEC)
import DDC.Core.Module
import Data.Char
import qualified DDC.Core.Tetra         as Tetra
import qualified DDC.Core.Salt          as Salt
import qualified DDC.Data.SourcePos     as BP


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
        :: String 
        -> Either Error 
                  (Interface (AnTEC BP.SourcePos Tetra.Name) 
                             (AnTEC BP.SourcePos Salt.Name))
parseInterface str
 = let  -- Attach line numbers to ach line
        ls      = lines str
        lsNum   = zip [1..] ls
   in   pInterface lsNum


-- | Parse an interface file.
pInterface :: Parser InterfaceAA
pInterface []
        = Left ErrorEmpty

pInterface ((n, str) : rest)
        -- Skip over blank lines
        | all (\c -> isSpace c || c == '\n') str
        = pInterface rest

        -- The interface needs to start with the magic words and version number.
        | ["ddc", "interface", version] <- words str
        = do    cs              <- pComponents rest

                modName <- case [m | m@ComponentMeta{} <- cs] of
                                [m]     -> return $ componentModuleName m
                                _       -> Left $ ErrorNoMeta

                mTetra  <- case [m | m@ComponentTetraModule{} <- cs] of
                                []      -> return Nothing
                                [m]     -> return $ Just $ componentTetraModule m
                                _       -> Left ErrorDuplicateCore

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


pComponents :: Parser [Component]
pComponents []
        = return []

pComponents _
        = Left $ ErrorParse 0
        
