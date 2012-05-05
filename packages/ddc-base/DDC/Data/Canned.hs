
module DDC.Data.Canned
        (Canned(..))
where

-- | This function has a show instance that prints \"CANNED\" for any contained
--   type. We use it to wrap functional fields in data types that we still want
--   to derive Show intances for.
data Canned a
        = Canned a

instance Show (Canned a) where
        show _ = "CANNED"
