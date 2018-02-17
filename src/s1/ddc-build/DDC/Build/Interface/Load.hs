
-- | Loader for DDC interface files.
module DDC.Build.Interface.Load
        ( loadInterface
        , Error (..)
        , InterfaceAA)
where
import DDC.Build.Interface.Codec.Text.Decode
import Data.Time.Clock

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



