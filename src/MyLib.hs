{-# LANGUAGE OverloadedStrings #-}

module MyLib (run, handleEcho) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (decodeStrict)
import Data.Aeson.Extra (encodeStrict)
import Data.ByteString.Char8 qualified as BS
import Env (Env, fromInit, getNextId, ourNodeId)
import Protocol (Body (Echo, EchoOk, InitOk, echo), Msg (Msg, body, dest, inReplyTo, msgId, src))
import System.IO (hFlush, stdout)

getMessage :: IO Msg
getMessage = do
  line <- BS.getLine
  return $ case decodeStrict line of
    Just x -> x
    Nothing -> error "Failed to decode message"

type EnvIO a = ReaderT Env IO a

sendMessage :: Msg -> IO ()
sendMessage msg = do
  BS.putStr bs
  -- flush so that stdout redirection works
	-- for some reason putStr does not flush the buffer
  hFlush stdout
  where
    bs = encodeStrict msg `BS.append` "\n"

constructReply :: Msg -> Body -> EnvIO Msg
constructReply msg replyBody = do
  env <- ask
  replyMessageId <- liftIO $ getNextId env
  return
    Msg
      { src = (ourNodeId env),
        dest = (src msg),
        msgId = Just replyMessageId,
        inReplyTo = (msgId msg),
        body = replyBody
      }

replyToMessage :: Msg -> Body -> EnvIO ()
replyToMessage msg replyBody = do
  replyMessage <- constructReply msg replyBody
  liftIO $ sendMessage replyMessage

replyTo :: Env -> Msg -> Body -> IO ()
replyTo env msg body = do
  sendMessage
    Msg
      { src = (ourNodeId env),
        dest = (src msg),
        -- msgId = Just replyMessageId,
        -- replies don't need msgId
        msgId = Nothing,
        inReplyTo = (msgId msg),
        body = body
      }

handleEcho :: EnvIO ()
handleEcho = do
  msg <- liftIO getMessage
  case body msg of
    Echo {echo} -> replyToMessage msg EchoOk {echo}
    _ -> return ()

initEnv :: IO Env
initEnv = do
  message <- getMessage
  env <- fromInit (body message)
  replyTo env message InitOk
  return env

loopWithEnv :: Env -> EnvIO () -> IO ()
loopWithEnv env handler = do
  runReaderT handler env
  loopWithEnv env handler

run :: EnvIO () -> IO ()
run handler = do
  env <- initEnv
  loopWithEnv env handler
