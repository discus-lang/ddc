
module DDC.Kernel.Transform.Example
where
import DDC.Kernel.Exp
import DDC.Kernel.Transform.Substitute
import Prelude hiding (abs)

data V  = V String
        deriving Eq

instance Show V where
        show (V str) = str


data O  = O
        deriving Show

data U u k  
        = U
        | K k

instance (Show u, Show k) => Show (U u k) where
        show  U    = "@"
        show (K k) = show k

-- Constructors ---------------------------------------------------------------
-- | Construct an application with a default annotation.
abs :: [b] -> [Exp a b u k] -> Exp a b u k -> Exp O b u k
abs bs xs x  
        = XAbs O bs (map (mapAnnot (const O)) xs) 
                    (mapAnnot (const O) x)

                
-- | Construct an application with an empty annotation.
app :: Exp a b u k -> [u] -> Exp O b u k
app x us = XApp O (mapAnnot (const O) x) us


-- | Construct a literal with a default annotation.
lit :: k -> Exp O b u k
lit     = XLit O


-- | Construct a named variable with the given name.
var :: u -> Exp O b u (U u k)
var u   = XApp O (XLit O U) [u]


kAdd    = K "add"


-- Examples -------------------------------------------------------------------
ex1     = app (abs ["x", "y", "z"] [lit (K "0")]
        $ app (lit kAdd) ["y", "z"]) ["i2", "i3"]


ex2     = app (abs ["a", "b", "c"] [var "y"]
        $ app (lit kAdd) ["a", "b"]) ["i2", "i3"]

