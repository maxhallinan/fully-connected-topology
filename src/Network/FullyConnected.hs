{-# LANGUAGE OverloadedStrings #-}

module Network.FullyConnected
  ( Address
  , Connections
  , Host
  , Port
  , broadcast
  , connect
  , listen
  , parseAddress
  , parseAddressList
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
import Data.List (drop)
import Data.Void (Void)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
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

parseAddress :: String -> Maybe Address
parseAddress = Mega.parseMaybe addressParser

parseAddressList :: String -> Maybe [Address]
parseAddressList = Mega.parseMaybe addressListParser

addressParser :: Parsec Address
addressParser = do
  hostString <- some (Mega.alphaNumChar <|> Mega.char '.')
  Mega.char ':'
  portString <- some Mega.digitChar
  return $ address (host hostString) (port portString)

addressListParser :: Parsec [Address]
addressListParser = addressParser `Mega.sepBy` Mega.char ','

type Connections = Concurrent.MVar [Net.Socket]

initConnections :: IO Connections
initConnections = Concurrent.newMVar []

type MessageHandler = String -> IO ()

listen :: MessageHandler -> Address -> IO ()
listen handleMessage ownAddress = Net.withSocketsDo $ do
  Exception.bracket
    (startServer ownAddress)
    closeSocket
    (handleConnections handleMessage)

broadcast :: Connections -> String -> IO ()
broadcast connections message = do
  sockets <- Concurrent.readMVar connections
  fold <$> traverse (sendMessage message) sockets
  where
    send :: String -> Net.Socket -> IO ()
    send message socket = void $ forkIO $ do
      sendMessage message socket

sendMessage :: String -> Net.Socket -> IO ()
sendMessage message peerSocket = do
  Exception.catch send handleError
  where
    send :: IO ()
    send = do
      let m           = ByteChar.pack message
          totalBytes  = ByteChar.length m
      bytesSent <- NetByte.send peerSocket m
      -- NetByte.send is not guaranteed to send all bytes
      if bytesSent < totalBytes
        then sendMessage (drop bytesSent message) peerSocket
        else return ()

    handleError :: IO.Error.IOError -> IO ()
    handleError error = do
      -- TODO: What is the best approach to error handling?
      -- Retry the send? Close the socket? Close, reconnect, and send?
      putStrLn $ "Error sending the message: " ++ (show error)

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

handleConnections :: MessageHandler -> Net.Socket -> IO ()
handleConnections handleMessage ownSocket = forever $ do
  (peerSocket, peerAddress) <- Net.accept ownSocket
  forkFinally
    (receiveMessage handleMessage (peerSocket, peerAddress))
    (\_ -> Net.close peerSocket)
  putStrLn $ "Now receiving messages from " ++ show peerAddress

receiveMessage :: MessageHandler -> (Net.Socket, Net.SockAddr) -> IO ()
receiveMessage handleMessage (peerSocket, peerAddress) = do
  message <- NetByte.recv peerSocket 1024
  if ByteChar.length message == 0
    then do
      closeSocket peerSocket
    else do
      handleMessage $ ByteChar.unpack message
      receiveMessage handleMessage (peerSocket, peerAddress)

connect :: [Address] -> IO Connections
connect addresses = do
  connections <- initConnections
  fold <$> traverse (connect connections) addresses
  return connections
  where
    connect :: Connections -> Address -> IO ()
    connect connections address = void $ forkIO $ do
      addrInfo <- resolveAddress address
      Exception.catch (connection addrInfo) (retryConnect addrInfo)
      where
        connection :: Net.AddrInfo -> IO ()
        connection addrInfo = connectToPeer handleConnection addrInfo

        handleConnection :: Net.Socket -> IO ()
        handleConnection c = do
          Concurrent.modifyMVar connections (updateConnections c)
          void $ forkIO $ cleanup connections c

        updateConnections :: Net.Socket -> [Net.Socket] -> IO ([Net.Socket], ())
        updateConnections c cs = return (c : cs, ())

        retryConnect :: Net.AddrInfo -> IO.Error.IOError -> IO ()
        retryConnect addrInfo _ = do
          putStrLn $ "Retrying connection to peer: " ++ (showAddrInfo addrInfo)
          void $ Concurrent.threadDelay 1000000
          void $ connect connections address

        -- Remove socket from Connections when the remote socket closes
        cleanup :: Connections -> Net.Socket -> IO ()
        cleanup connections peerSocket = do
          message <- NetByte.recv peerSocket 1024
          if ByteChar.length message == 0
            then do
              closeSocket peerSocket
              Concurrent.modifyMVar
                connections
                (\cs -> return (filter (/= peerSocket) cs, ()))
              putStrLn "Closed connection"
            else do
              cleanup connections peerSocket

connectToPeer :: (Net.Socket -> IO ()) -> Net.AddrInfo -> IO ()
connectToPeer handleConnection addrInfo = do
  Exception.bracketOnError socket close connect
  where
        peerIp = showAddrInfo addrInfo

        socket :: IO Net.Socket
        socket = Net.socket
          (Net.addrFamily addrInfo)
          (Net.addrSocketType addrInfo)
          (Net.addrProtocol addrInfo)

        connect :: Net.Socket -> IO ()
        connect peerSocket = do
          putStrLn $ "Connecting to peer: " ++ peerIp
          Net.connect peerSocket $ Net.addrAddress addrInfo
          handleConnection peerSocket
          putStrLn $ "Connected to peer: " ++ peerIp

        close :: Net.Socket -> IO ()
        close peerSocket = closeSocket peerSocket

showAddrInfo :: Net.AddrInfo -> String
showAddrInfo = show . Net.addrAddress
