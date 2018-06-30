
module DDC.Build.Interface.Base
        ( Interface (..))
where
import DDC.Core.Module
import Data.Time.Clock


-- | Module interface.
data Interface n
        = Interface
        { -- | Path that the interface was loaded from.
          interfaceFilePath     :: FilePath

          -- | Last modification time of the interface file,
          --   used to determine when the source needs to be rebuilt.
        , interfaceTimeStamp    :: UTCTime

        , interfaceVersion      :: String
        , interfaceModuleName   :: ModuleName
        , interfaceModule       :: Maybe (Module () n) }


