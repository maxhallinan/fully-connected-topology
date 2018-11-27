{-# LANGUAGE OverloadedStrings #-}

module Network.FullyConnected
  ( Address
  , Host
  , Port
  , address
  , host
  , listenToPeers
  , port
  , talkToPeers
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as Exception
import Control.Monad ((<=<), forever)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Coerce (coerce)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte


data Address = Address Host Port deriving (Eq)


instance Show Address where
  show (Address host port) = show host ++ ":" ++ show port


address :: Host -> Port -> Address
address = Address


newtype Host = Host String deriving (Eq)


instance Show Host where
  show = coerce


host :: String -> Host
host = Host


newtype Port = Port String deriving (Eq)


instance Show Port where
  show = coerce


port :: String -> Port
port = Port

type MessageHandler = (String -> IO ())

listenToPeers :: MessageHandler -> Address -> IO ()
listenToPeers handleMessage ownAddress = Net.withSocketsDo $ do
  Exception.bracket
    (startServer ownAddress)
    stopServer
    (handleConnections handleMessage)


talkToPeers :: [Address] -> String -> IO ()
talkToPeers peerAddresses message = undefined


-- Helper functions


startServer :: Address -> IO Net.Socket
startServer = listenAtAddress <=< resolveAddress


stopServer :: Net.Socket -> IO ()
stopServer = Net.close


resolveAddress :: Address -> IO Net.AddrInfo
resolveAddress (Address host port) = do
  let hints = Net.defaultHints { Net.addrFlags = [Net.AI_PASSIVE]
                               , Net.addrSocketType = Net.Stream
                               }
  (addressInfo : _) <- Net.getAddrInfo
                        (Just hints)
                        (Just $ coerce host)
                        (Just $ coerce port)
  return addressInfo


listenAtAddress :: Net.AddrInfo -> IO Net.Socket
listenAtAddress addressInfo = do
  socket <- Net.socket
              (Net.addrFamily addressInfo)
              (Net.addrSocketType addressInfo)
              (Net.addrProtocol addressInfo)
  Net.bind socket (Net.addrAddress addressInfo)
  -- set the max queue size for connection requests to 5
  Net.listen socket 5
  putStrLn $ "Now listening on " ++ (showAddrInfo addressInfo)
  return socket


handleConnections ::  MessageHandler -> Net.Socket -> IO ()
handleConnections handleMessage ownSocket = forever $ do
  (peerSocket, peerAddress) <- Net.accept ownSocket
  forkFinally
    (receiveMessages handleMessage (peerSocket, peerAddress))
    (\_ -> Net.close peerSocket)
  putStrLn $ "Now receiving messages from " ++ show peerAddress


receiveMessages :: MessageHandler -> (Net.Socket, Net.SockAddr) -> IO ()
receiveMessages handleMessage (peerSocket, peerAddress) = forever $ do
  message <- NetByte.recv peerSocket 1024
  handleMessage $ ByteChar.unpack message


showAddrInfo :: Net.AddrInfo -> String
showAddrInfo = show . Net.addrAddress
