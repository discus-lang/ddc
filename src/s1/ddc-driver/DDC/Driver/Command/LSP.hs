{-# LANGUAGE OverloadedStrings #-}

module DDC.Driver.Command.LSP
        ( cmdLSP
        , State (..)
        , Phase (..))
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import qualified System.IO      as S
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
-- | Become a language server, using the LSP protocol.
cmdLSP :: Maybe FilePath -> ExceptT String IO ()
cmdLSP mFileLog
 = do   state   <- lspStartup mFileLog
        loopRPC state


loopRPC state
 = do   txContentLength <- liftIO $ T.hGetLine S.stdin
        lspLog state $ "line length: " ++ show txContentLength

        txEmpty         <- liftIO $ T.hGetLine S.stdin
        lspLog state $ "line empty: " ++ show txEmpty

        txChunk         <- liftIO $ T.hGetChunk S.stdin
        lspLog state $ "chunk: " ++ show txChunk

        case statePhase state of
         PhaseStartup
           -> do
                handleStartup state txChunk
                loopRPC state

         _ ->   loopRPC state

        loopRPC state

handleStartup state txChunk
 | J.Ok jValue  <- J.decode $ T.unpack txChunk
 , Just req     <- unpackRequestMessage jValue
 , Just inits   <- unpackInitializeParams req
 = do
        lspLog state $ "* Initialized " ++ show inits
        return state

 | otherwise
 = return state { statePhase = PhaseInitFailed }


takeJust :: Maybe a -> Maybe a
takeJust x = x


takeRight :: Either a b -> Maybe b
takeRight e
 = case e of
        Left _  -> Nothing
        Right b -> Just b


takeObject :: J.JSValue -> Maybe [(String, J.JSValue)]
takeObject vv
 = case vv of
        J.JSObject fs   -> Just $ J.fromJSObject fs
        _               -> Nothing


takeString :: J.JSValue -> Maybe String
takeString vv
 = case vv of
        J.JSString js   -> Just $ J.fromJSString js
        _               -> Nothing


takeRational :: J.JSValue -> Maybe Rational
takeRational vv
 = case vv of
        J.JSRational _ r -> Just r
        _               -> Nothing


takeObjPathBool :: J.JSValue -> [String] -> Maybe (Maybe Bool)
takeObjPathBool jv []
 = case jv of
        J.JSBool b      -> Just (Just b)
        _               -> Nothing

takeObjPathBool jv (p : ps)
 = case jv of
        J.JSObject fs
         -> case lookup p (J.fromJSObject fs) of
                Nothing  -> Just Nothing
                Just jv' -> takeObjPathBool jv' ps

        _ -> Nothing


withNull :: J.JSValue -> (J.JSValue -> Maybe a) -> Maybe (Maybe a)
withNull vv f
 = case vv of
        J.JSNull        -> Just Nothing
        _               -> case f vv of
                                Nothing -> Nothing
                                Just x  -> Just (Just x)


---------------------------------------------------------------------------------------------------
type DocumentUri        = String


---------------------------------------------------------------------------------------------------
data RequestMessage
        = RequestMessage
        { rmId         :: Either Rational String
        , rmMethod     :: String
        , rmParams     :: Maybe (Either [J.JSValue] (J.JSObject J.JSValue)) }
        deriving Show


unpackRequestMessage :: J.JSValue -> Maybe RequestMessage
unpackRequestMessage jv
 = do   jfs     <- takeObject jv

        sVer    <- join $ fmap takeString $ lookup "jsonrpc" jfs
        guard   $ sVer == "2.0"

        jsId    <- lookup "id" jfs
        eId     <- case jsId of
                        J.JSRational _ r -> Just $ Left r
                        J.JSString s     -> Just $ Right (J.fromJSString s)
                        _                -> Nothing

        sMethod <- join $ fmap takeString $ lookup "method" jfs

        mePs    <- case lookup "params" jfs of
                        Just (J.JSArray vals)   -> Just $ Just $ Left vals
                        Just (J.JSObject ops)   -> Just $ Just $ Right ops
                        Just _                  -> Nothing
                        Nothing                 -> Just Nothing

        return  $ RequestMessage
                { rmId          = eId
                , rmMethod      = sMethod
                , rmParams      = mePs }


---------------------------------------------------------------------------------------------------
data InitializeParams
        = InitializeParams
        { ipProcessId           :: Maybe Int
        , ipRootPath            :: Maybe (Maybe String)
        , ipRootUri             :: Maybe DocumentUri
        , ipInitOptions         :: Maybe J.JSValue
        , ipClientCapabilities  :: ClientCapabilities }
--        , ipTrace               :: Maybe Trace
--        , ipWorkspaceFolders    :: J.JSValue }
        deriving Show


unpackInitializeParams :: RequestMessage -> Maybe InitializeParams
unpackInitializeParams rm
 = do
        fs      <- takeJust (rmParams rm) >>= takeRight >>= pure . J.fromJSObject

        jsPid   <- lookup "processId" fs
        miPid   <- fmap (fmap truncate) $ withNull jsPid takeRational

        mRPath  <- case lookup "rootPath" fs of
                        Nothing -> Just Nothing
                        Just js -> withNull js takeString >>= pure . Just

        mRUri   <- case lookup "rootUri" fs of
                        Nothing -> Nothing
                        Just js -> withNull js takeString

        jsOpts  <- Just $ lookup "initOptions" fs

        ccaps   <- join $ fmap unpackClientCapabilities
                $  lookup "capabilities" fs

        return  $ InitializeParams
                { ipProcessId           = miPid
                , ipRootPath            = mRPath
                , ipRootUri             = mRUri
                , ipInitOptions         = jsOpts
                , ipClientCapabilities  = ccaps }


---------------------------------------------------------------------------------------------------
data ClientCapabilities
        = ClientCapabilities
        { ccWorkspace           :: [WorkspaceClientCapability]
        , ccTextDocument        :: Maybe J.JSValue -- TextDocumentClientCapabilities
        , ccExperimental        :: Maybe J.JSValue }
        deriving Show


data WorkspaceClientCapability
        = WCApplyEdit
        | WCEditDocumentChanges
        | WCDidChangeConfigurationDR
        | WCDidChangeWatchedFilesDR
        | WCSymbolDR
        | WCSymbolKindValueSet [J.JSValue]
        | WCExecuteCommandDR
        | WCWorkspaceFolders
        | WCConfiguration
        deriving Show


unpackClientCapabilities :: J.JSValue -> Maybe ClientCapabilities
unpackClientCapabilities jv
 = do
        fs      <- takeObject jv

        mWs     <- case lookup "workspace" fs of
                        Nothing -> Just []
                        Just js -> unpackWorkspaceClientCapabilities js

        mTx     <- case lookup "textDocument" fs of
                        Nothing -> Just Nothing
                        Just js -> Just (Just js)

        jsAny   <- pure $ lookup "experimental" fs

        return  $ ClientCapabilities
                { ccWorkspace           = mWs
                , ccTextDocument        = mTx
                , ccExperimental        = jsAny }



unpackWorkspaceClientCapabilities :: J.JSValue -> Maybe [WorkspaceClientCapability]
unpackWorkspaceClientCapabilities jv
 = let  testCap c ps
         = case takeObjPathBool jv ps of
                Nothing                 -> Nothing
                Just (Just True)        -> Just (Just c)
                Just (Just False)       -> Just Nothing
                Just Nothing            -> Nothing

   in fmap catMaybes $ sequence
        [ testCap WCApplyEdit                   ["applyEdit"]
--        , testCap WCEditDocumentChanges         ["workspaceEdit", "documentChanges"]
--        , testCap WCDidChangeConfigurationDR    ["didChangeConfiguration", "dynamicRegistration"]
--        , testCap WCDidChangeWatchedFilesDR     ["didChangeWatchedFiles",  "dynamicRegistration"]
--        , testCap WCSymbolDR                    ["symbol", "dynamicRegistration"]
--        , testCap WCExecuteCommandDR            ["executeCommand", "dynamicRegistration"]
--        , testCap WCWorkspaceFolders            ["workspaceFolders"]
--        , testCap WCConfiguration               ["configuration"]
        ]


---------------------------------------------------------------------------------------------------
data Phase
        = PhaseStartup
        | PhaseInitFailed
        | PhaseInitialized


data State
        = State
        { stateLogDebug :: Maybe (FilePath, S.Handle)
        , statePhase    :: Phase }

type S a = ExceptT String IO a


lspStartup :: Maybe FilePath -> S State
lspStartup mFileLog
 = do
        mLogDebug
         <- case mFileLog of
                Nothing -> return Nothing
                Just filePath
                 -> do  hLogDebug <- liftIO $ S.openFile filePath S.WriteMode
                        return  $ Just (filePath, hLogDebug)
        let state
                = State
                { stateLogDebug = mLogDebug
                , statePhase    = PhaseStartup }

        lspLog state "* DDC Language Server Starting Up"
        return state


lspLog state (str :: String)
 | Just (_, h)  <- stateLogDebug state
 = do   liftIO $ S.hPutStr h (str ++ "\n")
        liftIO $ S.hFlush h

 | otherwise
 = return ()
