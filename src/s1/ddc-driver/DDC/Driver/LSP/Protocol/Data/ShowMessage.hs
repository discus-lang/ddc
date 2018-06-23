
module DDC.Driver.LSP.Protocol.Data.ShowMessage where
import DDC.Driver.LSP.Protocol.Pack


---------------------------------------------------------------------------------------------------
-- The show message request is sent from a server to a client to ask the
-- client to display a particular message in the user interface. In
-- addition to the show message notification the request allows to pass
-- actions and to wait for an answer from the client.
data ShowMessageRequestParams
        = ShowMessageRequestParams
        { smrType       :: MessageType
        , smrMessage    :: String
        , smrActions    :: Maybe [MessageActionItem] }
        deriving Show


instance Pack ShowMessageRequestParams where
 pack x
  = jobj $ catMaybes
        [ Just ("type",    pack $ smrType x)
        , Just ("message", pack $ smrMessage x)
        , fmap (\ms -> ("actions", packs ms)) $ smrActions x ]


---------------------------------------------------------------------------------------------------
data MessageActionItem
        = MessageActionItem
        { maiTitle      :: String }
        deriving Show

instance Pack MessageActionItem where
 pack x = jobj [ ("title", pack $ maiTitle x) ]


---------------------------------------------------------------------------------------------------
data MessageType
        = MtError       -- 1
        | MtWarning     -- 2
        | MtInfo        -- 3
        | MtLog         -- 4
        deriving (Show, Enum)

instance Pack MessageType where
 pack x = pack $ fromEnum x + 1

