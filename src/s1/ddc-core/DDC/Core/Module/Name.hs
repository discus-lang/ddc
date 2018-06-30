
module DDC.Core.Module.Name
        ( ModuleName    (..)
        , readModuleName
        , isMainModuleName
        , moduleNameMatchesPath

        , QualName      (..))
where
import Data.Typeable
import Data.List
import Control.DeepSeq
import DDC.Data.Pretty
import qualified Data.List              as List
import qualified System.FilePath        as System


-- ModuleName -------------------------------------------------------------------------------------
-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord, Typeable)

instance NFData ModuleName where
 rnf (ModuleName ss)
        = rnf ss


instance Pretty ModuleName where
 ppr (ModuleName parts)
        = string $ intercalate "." parts


-- | Read a string like 'M1.M2.M3' as a module name.
readModuleName :: String -> Maybe ModuleName
readModuleName []       = Nothing
readModuleName str
 = Just $ ModuleName $ go str
 where
        go s
         | elem '.' s
         , (n, '.' : rest)      <- span (/= '.') s
         = n : go rest

         | otherwise
         = [s]


-- | Check whether this is the name of the \"Main\" module.
isMainModuleName :: ModuleName -> Bool
isMainModuleName mn
 = case mn of
        ModuleName ["Main"]     -> True
        _                       -> False


-- | Check whether a module name matches the given file path of the module.
--
--   If the module is named M1.M2.M3 then the file needs to be called
--   PATH/M1/M2/M3.EXT for some base PATH and extension EXT.
--
moduleNameMatchesPath :: FilePath -> ModuleName -> Bool
moduleNameMatchesPath filePath (ModuleName mnParts)
 = checkParts (reverse fsParts) (reverse mnParts)
 where
        -- Split out the directory parts from the filename.
        fsParts
                = map (\f -> case List.stripPrefix "/" (reverse f) of
                                Just f' -> reverse f'
                                Nothing -> f)
                $ System.splitPath
                $ System.dropExtension filePath

        -- Check that the directory parts match the module name parts.
        checkParts [] _     = True
        checkParts _  []    = True
        checkParts (f: fs) (m : ms)
         | f == m           = checkParts fs ms
         | otherwise        = False


-- QualName ---------------------------------------------------------------------------------------
-- | A fully qualified name,
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show

instance NFData n => NFData (QualName n) where
 rnf (QualName mn n)
        = rnf mn `seq` rnf n

