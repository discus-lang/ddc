{-# LANGUAGE OverloadedStrings #-}

module DDC.Driver.Command.LSP
        ( cmdLSP
        , State (..)
        , Phase (..))
where
import DDC.Driver.LSP.Protocol.Data
import DDC.Driver.LSP.Protocol.Unpack
import DDC.Driver.LSP.Protocol.Pack

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified System.IO      as S
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Text.JSON      as J


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


---------------------------------------------------------------------------------------------------
-- | Become a language server, using the LSP protocol.
cmdLSP :: Maybe FilePath -> ExceptT String IO ()
cmdLSP mFileLog
 = do   state   <- lspStartup mFileLog
        loopRead state


loopRead state
 = do   lspLog state $ ". (waiting for message)"
        txContentLength <- liftIO $ T.hGetLine S.stdin
        lspLog state $ "> Received Message ---------------------------------"
        lspLog state $ "  line length: " ++ show txContentLength

        -- TODO: handle broken messages, don't just fail with pattern match.
        let Just txLength1  = T.stripPrefix "Content-Length: " txContentLength
        let Just txLength   = T.stripSuffix "\r" txLength1
        let lenChunk        = read (T.unpack txLength)

        lspLog state $ ". (waiting for newline)"
        txEmpty         <- liftIO $ T.hGetLine S.stdin
        lspLog state $ "  line empty: " ++ show txEmpty

        lspLog state $ ". (starting chunk read)"
        txChunk         <- loopReadChunk state lenChunk ""
        lspLog state $ "  chunk: " ++ T.unpack txChunk ++ "\n"

        case statePhase state of
         PhaseStartup
           -> loopInitialize state txChunk
         _ -> loopRead state


loopReadChunk :: State -> Int -> T.Text -> S T.Text
loopReadChunk state n acc
 | T.length acc >= n    = return acc
 | otherwise
 = do   lspLog state $ ". (waiting for chunk data)"
        moar    <- liftIO $ T.hGetChunk S.stdin
        loopReadChunk state n (T.append acc moar)


loopInitialize state txChunk
 | J.Ok jValue  <- J.decode $ T.unpack txChunk
 , Just req     <- unpackRequestMessage jValue
 , Just inits   <- unpackInitializeParams req
 = do
        lspLog state $ "* Received Initialization Request " ++ show inits ++ "\n"

        -- todo: convert req/response ids.
        lspSend $ pack $ responseSuccess 1
                $ InitializeResult
                $ serverCapabilitiesZero
                { scTextDocumentSync
                        = Just TextDocumentSyncOptions
                        { tdsoOpenClose         = Just True
                        , tdsoChange            = Just TdskIncremental
                        , tdsoWillSave          = Nothing
                        , tdsoWillSaveWaitUntil = Nothing
                        , tdsoSave              = Just $ SaveOptions
                                                { soIncludeText = Just True }
                        }

                , scHoverProvider
                        = Just True }

--      Sublime text doesn't LSP plugin handle the actions.
--        lspSend $ pack $ requestShowMessage 2
--                $ ShowMessageRequestParams
--                { smrType       = MtInfo
--                , smrMessage    = "derp"
--                , smrActions    = Just [MessageActionItem "derp", MessageActionItem "fish"] }

        lspLog state $ "* Initialized"
        loopRead $ state { statePhase = PhaseInitialized }

 | otherwise
 = do
        lspLog state $ "! Parse of initialization message failed"
        loopRead $ state { statePhase = PhaseInitFailed }


---------------------------------------------------------------------------------------------------
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


-- | Send a JSON value via JsonRPC to the client.
--   We print it to Stdout with the content-length headers.
lspSend :: J.JSValue -> S ()
lspSend js
 = liftIO
 $ do   let payload     = J.encode js
        S.putStr $ "Content-Length: " ++ show (length payload) ++ "\r\n"
        S.putStr $ "\r\n"
        S.putStr payload
        S.hFlush S.stdout


lspLog state (str :: String)
 | Just (_, h)  <- stateLogDebug state
 = do   liftIO $ S.hPutStr h (str ++ "\n")
        liftIO $ S.hFlush h

 | otherwise
 = return ()


---------------------------------------------------------------------------------------------------
-- requestShowMessage :: Int -> ShowMessageRequestParams -> Request ShowMessageRequestParams
-- requestShowMessage iid params
--         = Request (RequestIdInt iid) "window/showMessageRequest" (Just params)


responseSuccess :: Int -> result -> Response result ()
responseSuccess iid r
        = Response
        { responseId            = ResponseIdInt iid
        , responseResult        = Just r
        , responseError         = Nothing }


