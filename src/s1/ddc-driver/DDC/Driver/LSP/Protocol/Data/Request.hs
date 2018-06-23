
module DDC.Driver.LSP.Protocol.Data.Request where
import DDC.Driver.LSP.Protocol.Pack
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
-- | A request message to describe a request between the client and the
--   server. Every processed request must send a response back to the sender
--   of the request
data Request params
        = Request
        { requestId     :: RequestId
        , requestMethod :: String
        , requestParams :: Maybe params }
        deriving Show


instance Pack params => Pack (Request params) where
 pack x = jobj $ catMaybes
        [ Just ("id",           pack $ requestId x)
        , Just ("method",       pack $ requestMethod x)
        , fmap (\p -> ("params", pack p)) $ requestParams x ]


---------------------------------------------------------------------------------------------------
data RequestId
        = RequestIdInt    Int
        | RequestIdString String
        deriving Show


instance Pack RequestId where
 pack = \case
        RequestIdInt i          -> pack i
        RequestIdString s       -> pack s


---------------------------------------------------------------------------------------------------
data RequestMessage
        = RequestMessage
        { rmId          :: Either Rational String
        , rmMethod      :: String
        , rmParams      :: Maybe (Either [J.JSValue] (J.JSObject J.JSValue)) }
        deriving Show


