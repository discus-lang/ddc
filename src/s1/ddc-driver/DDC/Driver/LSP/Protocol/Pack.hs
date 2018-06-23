module DDC.Driver.LSP.Protocol.Pack
        ( Pack(..)
        , packs
        , jobj
        , J.JSValue(..)
        , module Data.Maybe)
where
import qualified Text.JSON      as J
import Data.Maybe


class Pack a where
 pack :: a -> J.JSValue

instance Pack () where
 pack _ = J.JSNull

instance Pack Bool where
 pack   = J.JSBool

instance Pack String where
 pack   = J.JSString . J.toJSString

instance Pack Int where
 pack i = J.JSRational False (toRational i)

packs :: Pack a => [a] -> J.JSValue
packs xs = J.JSArray $ map pack xs

jobj    = J.JSObject . J.toJSObject

