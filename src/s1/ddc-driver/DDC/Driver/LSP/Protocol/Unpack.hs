
-- | Unpacking of LSP JSON messages into Haskell data types.
--   We don't always unpack every field, particularly if we're not providing
--   the associated feature of the LSP protocol. See the LSP spec for full details.
module DDC.Driver.LSP.Protocol.Unpack where
import DDC.Driver.LSP.Protocol.Data
import DDC.Driver.LSP.Protocol.Parse
import Control.Monad
import Data.Maybe
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
-- | Unpack a `RequestMessage` from JSON.
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
-- | Unpack `InitializeParams` from a `RequestMessage`.
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
-- | Unpack `ClientCapacilities` from JSON.
unpackClientCapabilities :: J.JSValue -> Maybe ClientCapabilities
unpackClientCapabilities jv
 = do
        fs      <- takeObject jv

        mWs     <- case lookup "workspace" fs of
                        Nothing -> Just []
                        Just js -> unpackWorkspaceClientCapabilities js

        mTx     <- case lookup "textDocument" fs of
                        Nothing -> Just []
                        Just js -> unpackTextDocumentClientCapabilities js

        jsAny   <- pure $ lookup "experimental" fs

        return  $ ClientCapabilities
                { ccWorkspace           = mWs
                , ccTextDocument        = mTx
                , ccExperimental        = jsAny }


---------------------------------------------------------------------------------------------------
-- | Unpack `WorkspaceClientCapabilities` from JSON.
unpackWorkspaceClientCapabilities :: J.JSValue -> Maybe [WorkspaceClientCapability]
unpackWorkspaceClientCapabilities jv
 = let  -- Test for a capability flag in the JSON message goop.
        -- The message format is wierdly redundant in its optionality.
        testCap c ps
         = case takeObjPathBool jv ps of
                Nothing                 -> Nothing
                Just (Just True)        -> Just (Just c)
                Just (Just False)       -> Just Nothing
                Just Nothing            -> Just Nothing

   in fmap catMaybes $ sequence
        [ testCap WcApplyEdit
                ["applyEdit"]

        , testCap WcEditDocumentChanges
                ["workspaceEdit", "documentChanges"]

        , testCap WcDidChangeConfigurationDR
                ["didChangeConfiguration", "dynamicRegistration"]

        , testCap WcDidChangeWatchedFilesDR
                ["didChangeWatchedFiles",  "dynamicRegistration"]

        , testCap WcSymbolDR
                ["symbol", "dynamicRegistration"]

        , testCap WcExecuteCommandDR
                ["executeCommand", "dynamicRegistration"]

        , testCap WcWorkspaceFolders
                ["workspaceFolders"]

        , testCap WcConfiguration
                ["configuration"]
        ]


---------------------------------------------------------------------------------------------------
-- | Unpack `WorkspaceClientCapabilities` from JSON.
unpackTextDocumentClientCapabilities :: J.JSValue -> Maybe [TextDocumentClientCapability]
unpackTextDocumentClientCapabilities jv
 = let  -- Test for a capability flag in the JSON message goop.
        -- The message format is wierdly redundant in its optionality.
        testCap c ps
         = case takeObjPathBool jv ps of
                Nothing                 -> Nothing
                Just (Just True)        -> Just (Just c)
                Just (Just False)       -> Just Nothing
                Just Nothing            -> Just Nothing
   in fmap catMaybes $ sequence
        [ testCap TcSynchronizationDR
                ["synchronization", "dynamicRegistration"]

        , testCap TcSynchronizationWillSave
                ["synchronization", "willSave"]

        , testCap TcSynchronizationWillSaveUntil
                ["synchronization", "willSaveUntil"]

        , testCap TcSynchronizationDidSave
                ["synchronization", "didSave"]

        , testCap TcCompletionDR
                ["completion", "dynamicRegistration"]

        , testCap TcCompletionSnippet
                ["completion", "completionItem", "snippetSupport"]

        , testCap TcCompletionCommitCharacters
                ["completion", "completionItem", "commitCharactersSupport"]

        , testCap TcCompletionDeprecated
                ["completion", "completionItem", "deprecatedSupport"]

        , testCap TcHoverDR
                ["hover",              "dynamicRegistration"]

        , testCap TcSignatureHelpDR
                ["signatureHelp",      "dynamicRegistration"]

        , testCap TcReferencesDR
                ["references",         "dynamicRegistration"]

        , testCap TcDocumentHighlightDR
                ["documentHighlight",  "dynamicRegistration"]

        , testCap TcDocumentSymbolDR
                ["documentSymbol",     "dynamicRegistration"]

        , testCap TcFormattingDR
                ["formatting",         "dynamicRegistration"]

        , testCap TcRangeFormattingDR
                ["rangeFormatting",    "dynamicRegistration"]

        , testCap TcOnTypeFormattingDR
                ["onTypeFormatting",   "dynamicRegistration"]

        , testCap TcDefinitionDR
                ["definition",         "dynamicRegistration"]

        , testCap TcTypeDefinitionDR
                ["typeDefinition",     "dynamicRegistration"]

        , testCap TcImplementationDR
                ["implementation",     "dynamicRegistration"]

        , testCap TcCodeActionDR
                ["codeAction",         "dynamicRegistration"]

        , testCap TcCodeLensDR
                ["codeLens",           "dynamicRegistration"]

        , testCap TcDocumentLinkDR
                ["documentLink",       "dynamicRegistration"]

        , testCap TcColorProviderDR
                ["colorProvider",      "dynamicRegistration"]

        , testCap TcRenameDR
                ["rename",             "dynamicRegistration"]

        , testCap TcPublishDiagnosticsRelated
                ["publishDiagnostics", "relatedInformation"]
        ]



