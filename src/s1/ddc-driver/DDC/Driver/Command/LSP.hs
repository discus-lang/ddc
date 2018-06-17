{-# LANGUAGE OverloadedStrings #-}

module DDC.Driver.Command.LSP
        ( cmdLSP
        , State (..)
        , Phase (..))
where
import DDC.Driver.LSP.Protocol.Unpack

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


-- Startup ----------------------------------------------------------------------------------------
handleStartup state txChunk
 | J.Ok jValue  <- J.decode $ T.unpack txChunk
 , Just req     <- unpackRequestMessage jValue
 , Just inits   <- unpackInitializeParams req
 = do
        lspLog state $ "* Initialized " ++ show inits
        return state

 | otherwise
 = do
        lspLog state $ "! Parse of initialization message failed"
        return state { statePhase = PhaseInitFailed }



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
