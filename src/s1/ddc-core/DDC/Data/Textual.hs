
module DDC.Data.Textual
        ( Textual       (..)
        , (%), (%%)
        , T.Text)
where
import qualified Data.Text      as T
import qualified Data.List      as L


-------------------------------------------------------------------------------
class Textual a where
 -- | Construct an empty text thing.
 empty  :: a

 -- | Paste two texts together.
 paste  :: a -> a -> a

 -- | Paste two texts together, separated by space.
 pastes :: a -> a -> a

 -- | Generate a new line.
 line   :: a

 -- | Convert a character to text.
 char   :: Char -> a

 -- | Convert a string to text.
 string :: String -> a

 -- | Convert a Data.Text object to text.
 text   :: T.Text -> a

 -- | Convert an `Int` to text.
 int    :: Int  -> a

 -- | Convert an `Integer` to text.
 integer:: Integer -> a

 -- | Convert a `Float` to text.
 float  :: Float -> a

 -- | Convert a `Double` to text.
 double :: Double -> a

 -- | Concatentate text strings horizontally.
 hcat   :: [a] -> a
 hcat   = L.foldl' paste  empty

 -- | Separate text strings horizontally.
 hsep   :: [a] -> a
 hsep   = L.foldl' pastes empty

 -- | Concatenate text string vertically.
 vcat   :: [a] -> a
 vcat   = L.foldl' (\a b -> a `paste` line `paste` b) empty

 -- | Separate text strings horizontally.
 vsep   :: [a] -> a
 vsep   = L.foldl' (\a b -> a `paste` line `paste` line `paste` b) empty


-- | Alias for 'paste'
(%)  :: Textual a => a -> a -> a
(%) = paste
infixr 7 %


-- | Alias for 'pastes'
(%%) :: Textual a => a -> a -> a
(%%) = pastes
infixr 7 %%


-------------------------------------------------------------------------------
instance Textual String where
 empty          = ""
 paste  s1 s2   = s1 ++ s2
 pastes s1 s2   = s1 ++ " " ++ s2
 char           = show
 string         = id
 text           = T.unpack
 int            = show
 integer        = show
 float          = show
 double         = show


-------------------------------------------------------------------------------
instance Textual T.Text where
 empty          = T.empty
 paste  s1 s2   = s1 `mappend` s2
 pastes s1 s2   = mconcat [ s1, T.pack " ", s2]
 char   c       = T.pack [c]
 string         = T.pack
 text           = id
 int            = T.pack . show
 integer        = T.pack . show
 float          = T.pack . show
 double         = T.pack . show
 hcat xs        = mconcat xs
 hsep xs        = T.intercalate (T.pack " ")    xs
 vcat xs        = T.intercalate (T.pack "\n")   xs
 vsep xs        = T.intercalate (T.pack "\n\n") xs

