{-# LANGUAGE OverloadedStrings #-}

module Network.FullyConnected
  ( Address
  , Connections
  , Host
  , Port
  , addPeer
  , broadcast
  , connect
  , listen
  , parseAddress
  , removePeer
  ) where

import Control.Applicative ((<|>), some)
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Exception.Base as Exception
import Control.Monad ((<=<), forever, join, void)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.List (drop, find)
import Data.Void (Void)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

type Connections = Concurrent.MVar [Connection]

type Connection = (Address, Net.Socket)

initConnections :: IO Connections
initConnections = Concurrent.newMVar []

data Address = Address Host Port deriving (Eq)

instance Show Address where
  show (Address host port) = show host ++ ":" ++ show port

newtype Host = Host String deriving (Eq)

instance Show Host where
  show = coerce

newtype Port = Port String deriving (Eq)

instance Show Port where
  show = coerce

type Parsec = Mega.Parsec Void String

parseAddress :: String -> Maybe Address
parseAddress = Mega.parseMaybe addressParser

addressParser :: Parsec Address
addressParser = do
  hostString <- some (Mega.alphaNumChar <|> Mega.char '.')
  Mega.char ':'
  portString <- some Mega.digitChar
  return $ Address (Host hostString) (Port portString)

broadcast :: Connections -> String -> IO ()
broadcast connections message = do
  cs <- Concurrent.readMVar connections
  fold <$> traverse (send message) cs
  where
    send :: String -> Connection -> IO ()
    send message connection = void $ forkIO $ do
      sendMessage message connection

connect :: [Address] -> IO Connections
connect addresses = do
  connections <- initConnections
  fold <$> traverse (addPeer connections) addresses
  return connections

type MessageHandler = String -> IO ()

listen :: MessageHandler -> Address -> IO ()
listen handleMessage ownAddress = Net.withSocketsDo $ do
  Exception.bracket
    (startServer ownAddress)
    closeSocket
    (handleConnections handleMessage)

sendMessage :: String -> Connection -> IO ()
sendMessage message (address, peerSocket) = do
  Exception.catch send handleError
  where
    send :: IO ()
    send = do
      let m           = ByteChar.pack message
          totalBytes  = ByteChar.length m
      bytesSent <- NetByte.send peerSocket m
      -- NetByte.send is not guaranteed to send all bytes
      if bytesSent < totalBytes
        then sendMessage (drop bytesSent message) (address, peerSocket)
        else return ()

    handleError :: IO.Error.IOError -> IO ()
    handleError error = do
      -- TODO: What is the best approach to error handling?
      -- Retry the send? Close the socket? Close, reconnect, and send?
      putStrLn $ "Error sending message: " ++ (show error)

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
  (addrInfo : _) <- Net.getAddrInfo
                        (Just hints)
                        (Just $ coerce host)
                        (Just $ coerce port)
  return addrInfo

listenAtAddress :: Net.AddrInfo -> IO Net.Socket
listenAtAddress addrInfo = do
  socket <- Net.socket
              (Net.addrFamily addrInfo)
              (Net.addrSocketType addrInfo)
              (Net.addrProtocol addrInfo)
  Net.bind socket (Net.addrAddress addrInfo)
  -- set the max queue size for connection requests to 5
  Net.listen socket 5
  putStrLn $ "Listening on " ++ (showAddrInfo addrInfo)
  return socket

handleConnections :: MessageHandler -> Net.Socket -> IO ()
handleConnections handleMessage ownSocket = forever $ do
  (peerSocket, peerAddress) <- Net.accept ownSocket
  forkFinally
    (receiveMessage handleMessage peerSocket)
    (\_ -> Net.close peerSocket)
  putStrLn $ "Receiving messages from " ++ show peerAddress

receiveMessage :: MessageHandler -> Net.Socket -> IO ()
receiveMessage handleMessage peerSocket = do
  message <- NetByte.recv peerSocket 1024
  if ByteChar.length message == 0
    then do
      closeSocket peerSocket
    else do
      handleMessage $ ByteChar.unpack message
      receiveMessage handleMessage peerSocket

addPeer :: Connections -> Address -> IO ()
addPeer connections address = void $ forkIO $ do
  void $ Exception.bracketOnError peerSocket retryConnect connect
  where 
    peerSocket :: IO (Net.AddrInfo, Net.Socket)
    peerSocket = do 
      addrInfo <- resolveAddress address
      peerSocket <- createSocket addrInfo
      return (addrInfo, peerSocket)

    connect :: (Net.AddrInfo, Net.Socket) -> IO Net.Socket
    connect (addrInfo, peerSocket) = do
      putStrLn $ "Connecting to peer: " ++ (showAddrInfo addrInfo)
      Net.connect peerSocket $ Net.addrAddress addrInfo
      Concurrent.modifyMVar connections (add (address, peerSocket))
      forkIO $ listenForRemoteClose connections addrInfo peerSocket
      putStrLn $ "Connected to peer: " ++ (showAddrInfo addrInfo)
      return peerSocket

    add :: Connection -> [Connection] -> IO ([Connection], ())
    add c cs = return (c : cs, ())

    retryConnect :: (Net.AddrInfo, Net.Socket) -> IO ()
    retryConnect (addrInfo, peerSocket) = do
      putStrLn $ "Failed connection to peer: " ++ (showAddrInfo addrInfo)
      closeSocket peerSocket
      void $ Concurrent.threadDelay 1000000
      putStrLn $ "Retrying connection to peer: " ++ (showAddrInfo addrInfo)
      addPeer connections address

    listenForRemoteClose :: Connections -> Net.AddrInfo -> Net.Socket -> IO ()
    listenForRemoteClose connections addrInfo peerSocket = do
      message <- NetByte.recv peerSocket 1024
      if ByteChar.length message == 0
        then do
          -- Remove socket from Connections when the remote socket closes
          removePeer connections address
          putStrLn $ "Remotely closed connection to peer: " ++ (showAddrInfo addrInfo)
        else do
          listenForRemoteClose connections addrInfo peerSocket

removePeer :: Connections -> Address -> IO ()
removePeer connections address1 = do
  peerSocket <- Concurrent.modifyMVar connections remove
  case peerSocket of
    Just socket ->
      closeSocket socket
    Nothing ->
      return ()
  where remove cs = return (filter isConnection cs, snd <$> find isConnection cs)
        isConnection (address2, _) = address1 == address2

showAddrInfo :: Net.AddrInfo -> String
showAddrInfo = show . Net.addrAddress
