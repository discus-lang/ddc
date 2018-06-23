
module DDC.Driver.LSP.Protocol.Data.Initialize where
import DDC.Driver.LSP.Protocol.Pack
import DDC.Driver.LSP.Protocol.Data.Base
import DDC.Driver.LSP.Protocol.Data.ClientCapabilities
import DDC.Driver.LSP.Protocol.Data.ServerCapabilities


---------------------------------------------------------------------------------------------------
-- | The initialize request is sent as the first request from the client to the server.
data InitializeParams
        = InitializeParams
        { ipProcessId           :: Maybe Int
        , ipRootPath            :: Maybe (Maybe String)
        , ipRootUri             :: Maybe DocumentUri
        , ipInitOptions         :: Maybe JSValue
        , ipClientCapabilities  :: ClientCapabilities
--        , ipTrace               :: Maybe Trace
--        , ipWorkspaceFolders    :: [WorkspaceFolder]
        }
        deriving Show


---------------------------------------------------------------------------------------------------
data Trace
        = TraceOff
        | TraceMessages
        | TraceVerbose
        deriving Show


---------------------------------------------------------------------------------------------------
data WorkspaceFolder
        = WorkspaceFolder
        { wfUri         :: String
        , wfName        :: String }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | The server responds to the initialize request with an initialize result.
data InitializeResult
        = InitializeResult
        { irCapabilities        :: ServerCapabilities }
        deriving Show


instance Pack InitializeResult where
 pack ir
  = jobj
        [ ("capabilities", pack $ irCapabilities ir) ]
