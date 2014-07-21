
module DDC.Build.Interface.Base 
        ( Interface (..)
        , makeInterfaceTearLine
        , takeInterfaceTearLine
        , isInterfaceTearLine)
where
import DDC.Core.Module
import DDC.Core.Pretty
import DDC.Core.Compounds
import Data.Maybe
import qualified Data.Char              as Char
import qualified DDC.Core.Tetra         as Tetra
import qualified DDC.Core.Salt          as Salt


---------------------------------------------------------------------------------------------------
-- | Module interface.
data Interface ta sa
        = Interface
        { interfaceFilePath     :: FilePath
        , interfaceVersion      :: String
        , interfaceModuleName   :: ModuleName
        , interfaceTetraModule  :: Maybe (Module ta Tetra.Name)
        , interfaceSaltModule   :: Maybe (Module sa Salt.Name) }
        deriving Show


---------------------------------------------------------------------------------------------------
instance Pretty (Interface ta sa) where
 ppr i
  =     (text "ddc interface" <+> text (interfaceVersion i))
         <> line <> line
         <> vcat [ text $ makeInterfaceTearLine "Meta"
                 , text "module-meta"   <+> lbrace <> line
                        <> indent 8 (vcat
                                [ hsep [text "name:", ppr $ interfaceModuleName i ] ])
                        <> line <> rbrace]

        -- Include the full unoptimised core code in interface files.
        --   We include the unoptimised code so that the modules that inline
        --   functions from this one are not sensitive to the optimisation flags
        --   use here. If we included the optimised code it would make it harder
        --   to reason about the overall compilation process.
        <> (case interfaceTetraModule i of
                Just m  -> vcat [ line 
                                , text $ makeInterfaceTearLine "Tetra"
                                , ppr m ]
                Nothing -> empty)

        -- Include the header of the Salt module.
        --   We don't need the function definitions because inlining is performed
        --   using the higher level Tetra code.
        <> (case interfaceSaltModule i of
                Just m  -> 
                 -- zap the module body so we don't get the function definitions.
                 let m' = m { moduleBody = xUnit (annotOfExp $ moduleBody m) }
                 in  vcat [ line
                          , text $ makeInterfaceTearLine "Salt"
                          , ppr m' ]
                Nothing -> empty)


---------------------------------------------------------------------------------------------------
-- | Tear line to separate sections of the interface file. 
--   We use '~' because it's not used in the core language syntax.
makeInterfaceTearLine :: String -> String
makeInterfaceTearLine name
        = "~~ " ++ name ++ " " ++ replicate (80 - 4 - length name) '~'

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

