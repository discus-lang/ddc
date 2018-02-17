
module DDC.Build.Interface.Base
        (Interface (..))
where
import DDC.Core.Module
import Data.Time.Clock
import qualified DDC.Core.Discus        as Discus
import qualified DDC.Core.Salt          as Salt


-- | Module interface.
data Interface ta sa
        = Interface
        { -- | Path that the interface was loaded from.
          interfaceFilePath     :: FilePath

          -- | Last modification time of the interface file,
          --   used to determine when the source needs to be rebuilt.
        , interfaceTimeStamp    :: UTCTime

        , interfaceVersion      :: String
        , interfaceModuleName   :: ModuleName
        , interfaceDiscusModule :: Maybe (Module ta Discus.Name)
        , interfaceSaltModule   :: Maybe (Module sa Salt.Name) }


