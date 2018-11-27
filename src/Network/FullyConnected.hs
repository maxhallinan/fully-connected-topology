{-# LANGUAGE OverloadedStrings #-}

module Network.FullyConnected
  ( Address
  , Host
  , Port
  , address
  , addressParser
  , host
  , listenToPeers
  , port
  , talkToPeers
  ) where

import Control.Applicative ((<|>), some)
import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as Exception
import Control.Monad ((<=<), forever)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Void (Void)
import Data.Coerce (coerce)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

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

type Parsec = Mega.Parsec Void String

addressParser :: Parsec Address
addressParser = do
  hostString <- some (Mega.alphaNumChar <|> Mega.char '.')
  Mega.char ':'
  portString <- some Mega.digitChar
  return $ address (host hostString) (port portString)

type MessageHandler = (String -> IO ())

listenToPeers :: MessageHandler -> Address -> IO ()
listenToPeers handleMessage ownAddress = Net.withSocketsDo $ do
  Exception.bracket
    (startServer ownAddress)
    stopServer
    (handleConnections handleMessage)

talkToPeers :: [Address] -> String -> IO ()
talkToPeers peerAddresses message = do
  connections <- connectToPeers peerAddresses 
  broadcastMessage connections message

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

connectToPeers :: [Address] -> IO [Net.Socket]
connectToPeers peerAddresses = do
  connect peerAddresses
  where connect = traverse (connectToPeer <=< resolveAddress)

connectToPeer :: Net.AddrInfo -> IO Net.Socket
connectToPeer addressInfo = do
  putStrLn $ "Connecting to peer: " ++ (showAddrInfo addressInfo)
  peerSocket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
  Net.connect peerSocket $ Net.addrAddress addressInfo
  return peerSocket
  -- void $ forkFinally (connect peerSocket) (r peerSocket)
  -- where 
  --     r peerSocket e = do
  --       putStrLn $ show e
  --       retryConnect peerSocket

  --     --- what's happening at the moment is that I connect to the socket,
  --     --  send it the hello world message, and then the function finishes.
  --     --  so the thread finishes and the `r` function is called by forkFinally.
  --     --  This restarts the peer connection process.
  --     --  What I want is for the thread to sleep while it waits for a message
  --     --  to send.
  --     --  Could do this with an MVar.
  --     connect socket = do 
  --       Net.connect socket $ Net.addrAddress addressInfo
  --       NetByte.send socket "Hello, World!"
  --       putStrLn $ "Connected to " ++ (showAddrInfo addressInfo)

  --     retryConnect socket = do
  --       Net.close socket
  --       putStrLn $ "Retrying connection to: " ++ (showAddrInfo addressInfo)
  --       threadDelay 1500000
  --       connectToPeer addressInfo

broadcastMessage :: [Net.Socket] -> String -> IO ()
broadcastMessage message connections = undefined
  -- NetByte.send socket message

showAddrInfo :: Net.AddrInfo -> String
showAddrInfo = show . Net.addrAddress
