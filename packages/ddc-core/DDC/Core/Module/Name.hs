
module DDC.Core.Module.Name
        ( ModuleName    (..)
        , readModuleName
        , isMainModuleName

        , QualName      (..))
where
import Data.Typeable
import Control.DeepSeq


-- ModuleName -------------------------------------------------------------------------------------
-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord, Typeable)

instance NFData ModuleName where
 rnf (ModuleName ss)
        = rnf ss


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


-- QualName ---------------------------------------------------------------------------------------
-- | A fully qualified name, 
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show

instance NFData n => NFData (QualName n) where
 rnf (QualName mn n)
        = rnf mn `seq` rnf n

