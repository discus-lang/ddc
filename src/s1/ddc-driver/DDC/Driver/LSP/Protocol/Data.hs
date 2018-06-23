
module DDC.Driver.LSP.Protocol.Data
        ( module DDC.Driver.LSP.Protocol.Data.Base
        , module DDC.Driver.LSP.Protocol.Data.ClientCapabilities
        , module DDC.Driver.LSP.Protocol.Data.Initialize
        , module DDC.Driver.LSP.Protocol.Data.Response
        , module DDC.Driver.LSP.Protocol.Data.Request
        , module DDC.Driver.LSP.Protocol.Data.ServerCapabilities
        , module DDC.Driver.LSP.Protocol.Data.ShowMessage)
where
import DDC.Driver.LSP.Protocol.Data.Base
import DDC.Driver.LSP.Protocol.Data.ClientCapabilities
import DDC.Driver.LSP.Protocol.Data.Initialize
import DDC.Driver.LSP.Protocol.Data.Response
import DDC.Driver.LSP.Protocol.Data.Request
import DDC.Driver.LSP.Protocol.Data.ServerCapabilities
import DDC.Driver.LSP.Protocol.Data.ShowMessage


---------------------------------------------------------------------------------------------------
-- data NotificationMessage
--         = NotificationMessage
--         { nmMethod      :: String
--         , nmParams      :: Maybe (Either [J.JSValue] [(String, J.JSValue)]) }
--         deriving Show


---------------------------------------------------------------------------------------------------
-- | The log message notification is sent from the server to the client
--   to ask the client to log a particular message.
--   method: window/logMessage
-- data LogMessageParams
--         = LogMessageParams
--         { lmpType       :: MessageType
--         , lmpMessage    :: String }
--         deriving Show

