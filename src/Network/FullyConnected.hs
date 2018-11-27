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
import Control.Monad ((<=<), forever, void)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Foldable (fold)
import Data.Void (Void)
import Data.Coerce (coerce)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte
import qualified System.IO as IO
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
  Exception.bracket (startServer ownAddress) closeSocket (handleConnections handleMessage)

talkToPeers :: [Address] -> String -> IO ()
talkToPeers peerAddresses message = fold <$> traverse talk peerAddresses
  where talk = flip talkToPeer message

talkToPeer :: Address -> String -> IO ()
talkToPeer peerAddress message = do
  addrInfo <- resolveAddress peerAddress
  Exception.bracket (createSocket addrInfo) closeSocket (sendMessage addrInfo message)

-- TODO: concurrent message sending
sendMessage :: Net.AddrInfo -> String -> Net.Socket -> IO ()
sendMessage addrInfo message peerSocket = do
  Net.connect peerSocket $ Net.addrAddress addrInfo
  void $ NetByte.send peerSocket (ByteChar.pack message)

createSocket :: Net.AddrInfo -> IO Net.Socket
createSocket addrInfo =
  Net.socket
    (Net.addrFamily addrInfo)
    (Net.addrSocketType addrInfo)
    (Net.addrProtocol addrInfo)

startServer :: Address -> IO Net.Socket
startServer = listenAtAddress <=< resolveAddress

closeSocket :: Net.Socket -> IO ()
closeSocket = Net.close

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
    (receiveMessage handleMessage (peerSocket, peerAddress))
    (\_ -> Net.close peerSocket)
  putStrLn $ "Now receiving messages from " ++ show peerAddress

receiveMessage :: MessageHandler -> (Net.Socket, Net.SockAddr) -> IO ()
receiveMessage handleMessage (peerSocket, peerAddress) = do
  message <- NetByte.recv peerSocket 1024
  handleMessage $ ByteChar.unpack message

connectToPeer :: Net.AddrInfo -> IO Net.Socket
connectToPeer addressInfo = do
  putStrLn $ "Connecting to peer: " ++ (showAddrInfo addressInfo)
  peerSocket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
  Net.connect peerSocket $ Net.addrAddress addressInfo
  return peerSocket

showAddrInfo :: Net.AddrInfo -> String
showAddrInfo = show . Net.addrAddress
