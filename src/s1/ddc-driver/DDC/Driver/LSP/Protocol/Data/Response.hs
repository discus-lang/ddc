
module DDC.Driver.LSP.Protocol.Data.Response where
import DDC.Driver.LSP.Protocol.Pack
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
data Response result error
        = Response
        { responseId     :: ResponseId
        , responseResult :: Maybe result
        , responseError  :: Maybe (ResponseError error) }
        deriving Show


instance (Pack result, Pack error) => Pack (Response result error) where
 pack x = jobj $ catMaybes
        [ Just ("id", pack $ responseId x)
        , fmap (\r -> ("result", pack r)) $ responseResult x
        , fmap (\e -> ("error",  pack e)) $ responseError  x ]


---------------------------------------------------------------------------------------------------
data ResponseId
        = ResponseIdInt    Int
        | ResponseIdString String
        | ResponseIdNull
        deriving Show


instance Pack ResponseId where
 pack = \case
        ResponseIdInt i         -> pack i
        ResponseIdString s      -> pack s
        ResponseIdNull          -> J.JSNull


---------------------------------------------------------------------------------------------------
data ResponseError error
        = ResponseError
        { responseErrorCode     :: ResponseErrorCode
        , responseErrorMessage  :: String
        , responseData          :: Maybe error }
        deriving Show


instance Pack error => Pack (ResponseError error) where
 pack x = jobj $ catMaybes
        [ Just ("code",          pack     $ responseErrorCode x)
        , Just ("message",       pack     $ responseErrorMessage x)
        , fmap (\d -> ("error",  pack d)) $ responseData x ]


---------------------------------------------------------------------------------------------------
data ResponseErrorCode
        = RecParseError                 -- -32700
        | RecInvalidRequest             -- -32600
        | RecMethodNotFound             -- -32601
        | RecInvalidParams              -- -32602
        | RecInternalError              -- -32606
        | RecServerErrorStart           -- -32099
        | RecServerErrorEnd             -- -32000
        | RecServerNotInitialized       -- -32002
        | RecUnknownErrorCode           -- -32001
        | RecRequestCancelled           -- -32800
        deriving Show

instance Pack ResponseErrorCode where
 pack = \case
        RecParseError           -> pack (-32700 :: Int)
        RecInvalidRequest       -> pack (-32600 :: Int)
        RecMethodNotFound       -> pack (-32601 :: Int)
        RecInvalidParams        -> pack (-32602 :: Int)
        RecInternalError        -> pack (-32603 :: Int)
        RecServerErrorStart     -> pack (-32099 :: Int)
        RecServerErrorEnd       -> pack (-32000 :: Int)
        RecServerNotInitialized -> pack (-32002 :: Int)
        RecUnknownErrorCode     -> pack (-32001 :: Int)
        RecRequestCancelled     -> pack (-32800 :: Int)
