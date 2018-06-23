
module DDC.Driver.LSP.Protocol.Data.ServerCapabilities where
import DDC.Driver.LSP.Protocol.Pack
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
data ServerCapabilities
        = ServerCapabilities
        { scTextDocumentSync                    :: Maybe TextDocumentSyncOptions
        , scHoverProvider                       :: Maybe Bool
        , scCompletionProvider                  :: Maybe CompletionOptions
        , scSignatureHelpProvider               :: Maybe SignatureHelpOptions
        , scDefinitionProvider                  :: Maybe Bool
        , scTypeDefinitionProvider              :: Maybe Bool
        , scImplementationProvider              :: Maybe Bool
        , scReferencesProvider                  :: Maybe Bool
        , scDocumentHighlightProvider           :: Maybe Bool
        , scDocumentSymbolProvider              :: Maybe Bool
        , scWorkspaceSymbolProvider             :: Maybe Bool
        , scCodeActionProvider                  :: Maybe Bool
        , scCodeLensProvider                    :: Maybe CodeLensOptions
        , scDocumentFormattingProvider          :: Maybe Bool
        , scDocumentRangeFormattingProvider     :: Maybe Bool
        , scDocumentOnTypeFormattingProvider    :: Maybe DocumentOnTypeFormattingOptions
        , scRenameProvider                      :: Maybe Bool
        , scDocumentLinkProvider                :: Maybe DocumentLinkOptions
        , scColorProvider                       :: Maybe Bool
        , scExecuteCommandProvider              :: Maybe ExecuteCommandOptions
        , scWorkspaceFoldersSupported           :: Maybe Bool
        , scWorkspaceFoldersChangeNotifications :: Maybe (Either String Bool)
        , scExperimental                        :: Maybe J.JSValue }
        deriving Show


serverCapabilitiesZero :: ServerCapabilities
serverCapabilitiesZero
        = ServerCapabilities
        { scTextDocumentSync                    = Nothing
        , scHoverProvider                       = Nothing
        , scCompletionProvider                  = Nothing
        , scSignatureHelpProvider               = Nothing
        , scDefinitionProvider                  = Nothing
        , scTypeDefinitionProvider              = Nothing
        , scImplementationProvider              = Nothing
        , scReferencesProvider                  = Nothing
        , scDocumentHighlightProvider           = Nothing
        , scDocumentSymbolProvider              = Nothing
        , scWorkspaceSymbolProvider             = Nothing
        , scCodeActionProvider                  = Nothing
        , scCodeLensProvider                    = Nothing
        , scDocumentFormattingProvider          = Nothing
        , scDocumentRangeFormattingProvider     = Nothing
        , scDocumentOnTypeFormattingProvider    = Nothing
        , scRenameProvider                      = Nothing
        , scDocumentLinkProvider                = Nothing
        , scColorProvider                       = Nothing
        , scExecuteCommandProvider              = Nothing
        , scWorkspaceFoldersSupported           = Nothing
        , scWorkspaceFoldersChangeNotifications = Nothing
        , scExperimental                        = Nothing
        }


instance Pack ServerCapabilities where
 pack x
  = jobj $ catMaybes
        [ fmap (\e -> ("textDocumentSync",  pack e)) $ scTextDocumentSync x
        , fmap (\b -> ("hoverProvider",     pack b)) $ scHoverProvider x ]


---------------------------------------------------------------------------------------------------
data TextDocumentSyncOptions
        = TextDocumentSyncOptions
        { tdsoOpenClose         :: Maybe Bool
        , tdsoChange            :: Maybe TextDocumentSyncKind
        , tdsoWillSave          :: Maybe Bool
        , tdsoWillSaveWaitUntil :: Maybe Bool
        , tdsoSave              :: Maybe SaveOptions }
        deriving Show


instance Pack TextDocumentSyncOptions where
 pack x = jobj $ catMaybes $
        [ fmap (\b -> ("openClose",             pack b)) $ tdsoOpenClose x
        , fmap (\k -> ("change",                pack k)) $ tdsoChange x
        , fmap (\b -> ("willSave",              pack b)) $ tdsoWillSave x
        , fmap (\b -> ("willSaveWaitUntil",     pack b)) $ tdsoWillSaveWaitUntil x
        , fmap (\o -> ("save",                  pack o)) $ tdsoSave x ]


---------------------------------------------------------------------------------------------------
-- We derive Enum, but note that in the protocol the first kind
-- is represented by value 0, not 1 as per the derived instance.
data TextDocumentSyncKind
        = TdskNone              -- 1
        | TdskFull              -- 2
        | TdskIncremental       -- 3
        deriving (Show, Enum)


instance Pack TextDocumentSyncKind where
 pack x = pack $ fromEnum x + 1


---------------------------------------------------------------------------------------------------
data CompletionOptions
        = CompletionOptions
        { coResolveProvider     :: Maybe Bool
        , coTriggerCharacters   :: Maybe [String] }
        deriving Show


---------------------------------------------------------------------------------------------------
data SignatureHelpOptions
        = SignatureHelpOptions
        { shoTriggerCharacters  :: Maybe [String] }
        deriving Show


---------------------------------------------------------------------------------------------------
data CodeLensOptions
        = CodeLensOptions
        { cloResolveProvider    :: Maybe Bool }
        deriving Show


---------------------------------------------------------------------------------------------------
data DocumentOnTypeFormattingOptions
        = DocumentOnTypeFormattingOptions
        { dotfoFirstTriggerCharacter    :: String
        , dotfoMoreTriggerCharacter     :: Maybe [String] }
        deriving Show


---------------------------------------------------------------------------------------------------
data DocumentLinkOptions
        = DocumentLinkOptions
        { dloResolveProvider :: Maybe Bool }
        deriving Show


---------------------------------------------------------------------------------------------------
data ExecuteCommandOptions
        = ExecuteCommandOptions
        { ecoCommands   :: Maybe [String] }
        deriving Show


---------------------------------------------------------------------------------------------------
data SaveOptions
        = SaveOptions
        { soIncludeText :: Maybe Bool }
        deriving Show


instance Pack SaveOptions where
 pack x = jobj $ catMaybes
        [ fmap (\b -> ("includeText", pack b)) $ soIncludeText x ]
