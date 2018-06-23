
module DDC.Driver.LSP.Protocol.Data.ClientCapabilities where
import qualified Text.JSON      as J


---------------------------------------------------------------------------------------------------
data ClientCapabilities
        = ClientCapabilities
        { ccWorkspace    :: [WorkspaceClientCapability]
        , ccTextDocument :: [TextDocumentClientCapability]
        , ccExperimental :: Maybe J.JSValue }
        deriving Show


---------------------------------------------------------------------------------------------------
data WorkspaceClientCapability
        = WcApplyEdit
        | WcEditDocumentChanges
        | WcDidChangeConfigurationDR
        | WcDidChangeWatchedFilesDR
        | WcSymbolDR
        | WcSymbolKindValueSet [SymbolKind]
        | WcExecuteCommandDR
        | WcWorkspaceFolders
        | WcConfiguration
        deriving Show


---------------------------------------------------------------------------------------------------
data TextDocumentClientCapability
        = TcSynchronizationDR
        | TcSynchronizationWillSave
        | TcSynchronizationWillSaveUntil
        | TcSynchronizationDidSave
        | TcCompletionDR
        | TcCompletionSnippet
        | TcCompletionCommitCharacters
        | TcCompletionDocumentationFormat       [MarkupKind]
        | TcCompletionDeprecated
        | TcCompletionKindValueSet              [CompletionItemKind]
        | TcCompletionContextSupport
        | TcHoverDR
        | TcHoverContentFormat                  [MarkupKind]
        | TcSignatureHelpDR
        | TcSignatureHelpInformationFormat      [MarkupKind]
        | TcReferencesDR
        | TcDocumentHighlightDR
        | TcDocumentSymbolDR
        | TcDocumentSymbolKindValueSet          [SymbolKind]
        | TcFormattingDR
        | TcRangeFormattingDR
        | TcOnTypeFormattingDR
        | TcDefinitionDR
        | TcTypeDefinitionDR
        | TcImplementationDR
        | TcCodeActionDR
        | TcCodeActionLiteralKindValueSet       [CodeActionKind]
        | TcCodeLensDR
        | TcDocumentLinkDR
        | TcColorProviderDR
        | TcRenameDR
        | TcPublishDiagnosticsRelated
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Markup Kinds.
data MarkupKind
        = MkPlainText
        | MkMarkdown
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Symbol Kinds.
--   We derive Enum, but note that in the spec the first symbol kind
--   is represented by value 0, not 1 as per the derived instance.
data SymbolKind
        = SkFile                -- 1
        | SkModule              -- 2
        | SkNamespace           -- 3
        | SkPackage             -- ...
        | SkClass
        | SkMethod
        | SkProperty
        | SkField
        | SkConstructor
        | SkEnum
        | SkInterface
        | SkFunction
        | SkVariable
        | SkConstant
        | SkString
        | SkNumber
        | SkBoolean
        | SkArray
        | SkObject
        | SkKey
        | SkNull
        | SkEnumMember
        | SkStruct
        | SkEvent
        | SkOperator            -- 25
        | SkTypeParameter       -- 26
        deriving (Show, Enum)


---------------------------------------------------------------------------------------------------
-- | Completion Item Kinds.
--   We derive Enum, but note that in the protocol the first kind
--   is represented by value 0, not 1 as per the derived instance.
data CompletionItemKind
        = CkText                -- 1
        | CkMethod              -- 2
        | CkFunction            -- 3
        | CkConstructor         -- ...
        | CkField
        | CkVariable
        | CkClass
        | CkInterface
        | CkModule
        | CkProperty
        | CkUnit
        | CkValue
        | CkEnum
        | CkKeyword
        | CkSnippet
        | CkColor
        | CkFile
        | CkReference
        | CkFolder
        | CkEnumMember
        | CkConstant
        | CkStruct
        | CkEvent
        | CkOperator            -- 24
        | CkTypeParameter       -- 25
        deriving (Show, Enum)


---------------------------------------------------------------------------------------------------
data CodeActionKind
        = AkQuickFix                    -- 'quickfix'
        | AkRefactor                    -- 'refactor'
        | AkRefactorExtract             -- 'refactor.extract'
        | AkRefactorInline              -- 'refactor.inline'
        | AkRefactorRewrite             -- 'refactor.rewrite'
        | AkSource                      -- 'source'
        | AkSourceOrganizeImports       -- 'source.organizeImports'
        deriving Show

